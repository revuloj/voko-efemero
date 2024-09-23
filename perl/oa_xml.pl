#!/usr/bin/perl

# (c) 2024 ĉe Wolfram Diestel
# laŭ GPL 2.0
#
# enmetas la oficialecon laŭ listo el tekstdosiero
# en la artikolojn ( nur por unufoja uzo :)
# uzante XML::LibXML 
#
# agordu numeron de OA kaj fontdosieron
# donu artikolojn adaptendajn kiel argumento (uzante ĵokerojn):
#
#  perl oa_xml.pl a*.xml

use XML::LibXML;
# https://metacpan.org/pod/XML::LibXML
#use Text::CSV qw( csv );

use utf8;
binmode(STDOUT, "encoding(UTF-8)");

my $debug = 0; # 0..2

my $oa = '10';
my $listo = '../../voko-efemero/vrt/OA10.txt';

unless ($#ARGV>-1) {
    print "\n=> Certigu, ke vi troviĝas en la dosierujo kie enestas la artikoloj al kiuj\n";
    print "vi volas aldoni tradukojn el CSV-dosiero. Poste voku tiel:\n";
    print "   perl oa_xml.pl <art>*.xml...\n\n" ;
    exit 1;
}

my @artikoloj = @ARGV;

my $ofc_xpath = XML::LibXML::XPathExpression
    ->new(".//ofc");

my $OA; # hash: rad->[v1,v2,...]
read_txt($listo);

if ($debug) {
    my $afgan = $OA->{afgan};
    print(join(',',@{$afgan})."\n");
}

my $dosiero;
my %radikoj;
my %drvmap;
my %mrkmap;
my $doc;

#if ($debug) {
#    print "abako: ",$tradukoj->{abako},"\n";
#    print "absciso: ",$tradukoj->{absciso},"\n";
#}
#exit 1;

for $art (@artikoloj) {
    if ($art =~ /([a-z0-9]+)\.xml/) {
        $dosiero = $1;
    };
    process_art($art);
}

sub process_art {
    my $artikolo = shift;
    
    # ni reskribas ĉion al la sama artikolo, kiam ni
    # uzas git-versiadon!
    my $artout = $artikolo; #.".out";
    my $modified = 0;

    print "### ",uc($artikolo)," ###\n";

    %radikoj = ();
    %drvmap = ();
    %kapmap = ();
    %mrkmap = ();

    # load XML
    # DTD devas troviĝi relative al la XML-pado: ../dtd/*.dtd
    # alternative oni devus deklari ext_ent_handler
    # kiel klarigita en https://metacpan.org/pod/distribution/XML-LibXML/lib/XML/LibXML/Parser.pod#Parser-Options
    $doc = XML::LibXML->load_xml(location => $artikolo, expand_entities=>0, keep_blanks=>1);
    #open my $fh, '<', $test_art;
    #binmode $fh; # drop all PerlIO layers possibly created by a use open pragma
    #my $doc = XML::LibXML->load_xml(IO => $fh, validation=>0, expand_entities=>0, keep_blanks=>1);

    # nun ni povas uzi $doc (DOM) kiel klarigita en
    # https://metacpan.org/pod/distribution/XML-LibXML/lib/XML/LibXML/Document.pod
    # https://metacpan.org/pod/distribution/XML-LibXML/lib/XML/LibXML/Node.pod
    # https://metacpan.org/pod/distribution/libxml-enno/lib/XML/DOM/NamedNodeMap.pod

    # trovu art@mrk kaj altigu la version...
    my $art_mrk = $doc->findnodes('//art/@mrk')->[0];
    print ("nuna id: ".$art_mrk->value()."\n") if ($debug);
    my $new_id = incr_ver($art_mrk->value());
    $art_mrk->setValue($new_id);
    print ("nova id: ".$art_mrk->value()."\n") if ($debug);
    ### $modified=1; goto WRITE;

    # trovu radikojn (inkluzive de var-iaĵoj)
    my @rad = $doc->findnodes('//rad');
    for my $rad (@rad) {
        $radikoj->{var_key($rad)} = $rad->textContent();
        print var_key($rad).": ".$rad->textContent()."\n" if ($debug);
    }
        
    # trovu kapojn de la artikolo
    for my $a ($doc->findnodes('//art')) {
        extract_kap($a);
    }

    # trovu kapojn de derivaĵoj kaj anstataŭigu tildojn
    for my $d ($doc->findnodes('//drv')) {
        extract_kap($d);
    }
    print "kap: ".join(';',keys(%kapmap))."\n" if ($debug);
    print "drv: ".join(';',keys(%drvmap))."\n" if ($debug);

    # trovu ĉiujn elementojn kun @mrk
    if ($kun_mrk) {
        for my $m ($doc->findnodes('//*[@mrk]')) {
            extract_mrk($m);
        }
    }   

    my @kapvortoj = oa_rad(@rad);
    print "oa: ".join(',',@kapvortoj)."\n" if ($debug);

    # Laŭ kapvortoj art/kap ni rigardu ĉu estas en la listo oficialaj kaj se jes ni iru al drv
    # kaj provos aldoni <ofc>$OA</ofc>
    for my $k (keys(%kapmap)) {
        print "art/kap: |$k|...\n" if ($debug);
        if( $k ~~ @kapvortoj ) {
            my $mod = aldonu_ofc($k,\%kapmap);
            $modified ||= $mod;      

            # forigu por ne aldonu al la egala drv poste
            @kapvortoj = grep {$_ ne $k} @kapvortoj;      
        }
    }

    # Laŭ kapvortoj drv/kap ni rigardu ĉu estas en la listo oficialaj kaj se jes ni iru al drv
    # kaj provos aldoni <ofc>$OA</ofc>
    for my $k (keys(%drvmap)) {
        print "drv/kap: |$k|...\n" if ($debug);
        if( $k ~~ @kapvortoj ) {
            my $mod = aldonu_ofc($k,\%drvmap);
            $modified ||= $mod;    

            # forigu por fine vidu la netraktitajn
            @kapvortoj = grep {$_ ne $k} @kapvortoj;      
        }
    }

    print "NETROVITA: ".join(',',@kapvortoj)."\n" if (@kapvortoj);

    # nur skribu, se ni efektive aldonis tradukojn, ĉar
    # ankaŭ ŝanĝiĝas iom linirompado kaj kodado de unikodaj literoj en la XML
WRITE:    
    if ($modified) {
        open OUT, ">", $artout || die "Ne povas skribi al '$artout': $!\n";
        print OUT $doc;
        close OUT;
    }
}    

############ helpaj funkcioj.... #############

sub aldonu_ofc {
    # kap
    my $k = shift;
    my $map = shift;

    my $el = $map->{$k};
    my $modified = 0;

    if ($el) {

        my $inserted = 0;
        my $ignore = 0;

        # unue ni kontrolu ĉu en la derivaĵo jam estas koncerna oficialeco
        # se jes ni ne tuŝos ĝin.
        my $ofc = $el->find($ofc_xpath);
        
        if ($ofc) { # && $el->textContent eq $OA) {

            # se jam enestas iuj tradukoj ni ne aldonas...
            $ignore = 1;
            print "!!! jam enestas ofc '$oa' en '$k' !!!\n" if ($debug);

        } else {
            # ne enestas jam tradukoj serĉu kie enŝovi la novan tradukon

            # kreu <ofc> 
            my $te, $nl;
            $te = make_ofc();            
            #$nl = XML::LibXML::Text->new("\n  ");

            for $ch ($el->childNodes()) {
                if ($ch->nodeName eq 'kap') {
                    $ch->insertBefore($te,$ch->firstChild);

                    print "$k: +ofc ".$oa."\n";
                    $inserted = 1;
                    $modified = 1;

                    last;
                }                
            } # for
        } # else
    } # if $el

    return $modified;
}

sub oa_rad {
    # trovu en OA vortojn sur unu el la donitaj radikoj
    my @rad = @_;
    my @vortoj = ();
    for my $r (@rad) {
        my $rad = lc($r->textContent);
        push @vortoj, @{$OA->{$rad}};
    }
    return @vortoj;
}

# eltrovu atributon var el <rad resp. <tld
sub var_key {
    my $el = shift;
    my $var = $el->attributes()->getNamedItem('var');
    if ($var) { return $var->textContent() } else { return '_' };
}

sub attr {
    my ($el,$atr) = @_;
    my $a = $el->attributes()->getNamedItem($atr);
    if ($a) { return $a->textContent };
}

# kreu novan elementon inkl. de atributoj
sub make_el{
    my ($name,%attr) = @_;
    my $el = $doc->createElement($name);    
    while (($key, $val) = each %attr) {
        $el->setAttribute( $key, $val);
    }
    return $el;
}

# kreu unuopan ofc-elementon
sub make_ofc {
    my $el = make_el('ofc');
    $el->appendText($oa);
    return $el;
}

# trovu ĉiujn kapvortojn inkl. variaĵojn kaj referencu la derivaĵon ($node)
# sub tiuj kapvortoj

sub extract_kap {
    my $node = shift;
    my $res = '';

    my $kap = ($node->findnodes('kap'))[0];
    print "kap: ".$kap if ($debug);

    for my $ch ($kap->childNodes()) {
        print "ch ".$ch->nodeName.": ".$ch->textContent()."\n" if ($debug>=2);
        # se la ido estas tildo, ni anstataŭigu per la koncerna radiko / variaĵo
        if ($ch->nodeName eq 'tld') {            
            print "\n".$radikoj->{var_key($ch)}."\n" if ($debug); 
            my $tld = $radikoj->{var_key($ch)};
            my $lit = $ch->attributes()->getNamedItem('lit');
            if ($lit) {
                $tld = $lit->textContent . substr($tld,1)
            };
            $res .= $tld; 
        # se temas pri variaĵo ni rikure vokas extract_kap por trakti ĝin
        } elsif ($ch->nodeName eq 'var') {
            my $var = extract_kap($ch);
            # registru la derivaĵon ($node) sub la nomo $var
            $drvmap{$var} = $node;
        # se temas pri radiko ni aldonas ĝian text-enhavon
        } elsif ($ch->nodeName eq 'rad') {
            $res .= $ch->textContent();        
        # tekstojn kaj literunuojn ni kolektas kiel tekstenhavo
        } elsif ($ch->nodeType eq XML_TEXT_NODE || $ch->nodeType eq XML_ENTITY_REF_NODE) {
            print $ch."\n" if ($debug);
            my $cnt = $ch->textContent();
            $cnt =~ s/[\/,]//g;
            $res .= $cnt;
        } else {
            print "NT: ".$ch->nodeType."\n" if ($debug && $ch->nodeType ne XML_ELEMENT_NODE);
        }
    };
    # registru la derivaĵon ($node) sub la kapvorto $res
    $res =~ s/^\s+|\s+$//sg;
    $drvmap{$res} = $node if ($node->nodeName() eq 'drv');
    $kapmap{$res} = $node if ($node->nodeName() eq 'art');
    return $res;
}

sub extract_mrk {
    my $node = shift;
    my $mrk = attr($node,'mrk');
    $mrkmap{$mrk} = $node;
}

sub read_txt {
	my ($txtfile) = @_;

    open $TXT,"<:encoding(utf8)",$txtfile or die "Ne povis malfermi CSV '$txtfile': $!\n";
    my @vortoj = <$TXT>; 
    close $TXT;

    for my $v (@vortoj) {
        chomp $v;
        my @parts = split(/[\s\-]/,$v); 
        my $last = $parts[-1];
        my @vparts = split('/',$last);
        my $rad = lc($vparts[0]);

        print "OA: ".$rad."\n" if ($debug);

        #unless ($OA->{$rad}) {
        #    $OA->{$rad} = [$v]
        #} else {
        my @vj = @{$OA->{$rad}};
        $v =~ s|/||g;
        push @vj, ($v);
        $OA->{$rad} = \@vj;
        #};
    }
}

#sub dump_kapvortoj {
#    for $k (keys %{$kapvortoj}) {
#        print "$k: ".join(',',@{$kapvortoj->{$k}})."\n";
#    }
#}

sub in_array {
    my $el = shift;
    return (grep { $el eq $_} @_);
}


# La version ni eltrovos kaj altigos je unu kaj reskribas en la artikolon
# krome la artikolo piede enhavas komenton kun $Log$ por protokoli la lastajn ŝanĝoj
# ni aldonos supre la novan version kaj evt-e mallongigas la komenton
sub incr_ver {
	my ($id_mrk) = @_;

	# $Id: test.xml,v 1.51 2019/12/01 16:57:36 afido Exp $
	$id_mrk =~ m/\$Id:\s+([^\.]+)\.xml,v\s+(\d)\.(\d+)\s+(?:\d\d\d\d\/\d\d\/\d\d\s+\d\d:\d\d:\d\d)(.*?)\$/s;	
	my $ver = id_incr($2,$3);
	my $id = '$Id: '.$1.'.xml,v '.$ver.$4.'$';
	$id_mrk =~ s/\$Id:[^\$]+\$/$id/;
	#$art =~ s/\$Log[^\$]*\$(.*?)-->/log_incr($1,$ver,$shangh_file)/se;

	return $id_mrk;
}

# altigi la version je .1 kaj alpendigi la aktualan daton 
sub id_incr {
	my ($major,$minor) = @_;
	my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = gmtime(time);
	my $now = sprintf("%04d/%02d/%02d %02d:%02d:%02d", $year+1900, $mon+1, $mday, $hour, $min, $sec);
	return "$major.". ( ++$minor )." $now";
}