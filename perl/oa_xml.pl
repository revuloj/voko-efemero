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

my $debug = 1;

my $OA = '10';
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

my @kapvortoj = read_txt($listo);

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
        
    # trovu kapojn de derivaĵoj kaj anstataŭigu tildojn
    for my $d ($doc->findnodes('//drv')) {
        extract_kap($d);
    }
    print "drv: ".join(';',keys(%drvmap))."\n" if ($debug);

    # trovu ĉiujn elementojn kun @mrk
    if ($kun_mrk) {
        for my $m ($doc->findnodes('//*[@mrk]')) {
            extract_mrk($m);
        }
    }   

    # Laŭ kapvortoj ni rigardu ĉu estas en la listo oficialaj kaj se jes ni iru al drv
    # kaj provos aldoni <ofc>$OA</ofc>
    for my $k (keys(%drvmap)) {
        print "kap: |$k|...\n" if ($debug);
        if( $k ~~ @kapvortoj ) {
            my $mod = aldonu_ofc($k);
            $modified ||= $mod;            
        }
    }

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

    my $el = %drvmap{$k};
    my $modified = 0;

    if ($el) {

        my $inserted = 0;
        my $ignore = 0;

        # unue ni kontrolu ĉu en la derivaĵo jam estas koncerna oficialeco
        # se jes ni ne tuŝos ĝin.
        my $ofc = $el->find($ofc_xpath);
        
        if ($ofc && $el->textContent eq $OA) {

            # se jam enestas iuj tradukoj ni ne aldonas...
            $ignore = 1;
            print "!!! jam enestas ofc '$OA' !!!\n" if ($debug);

        } else {
            # ne enestas jam tradukoj serĉu kie enŝovi la novan tradukon

            # kreu <ofc> 
            my $te, $nl;
            $te = make_ofc();            
            #$nl = XML::LibXML::Text->new("\n  ");

            for $ch ($el->childNodes()) {
                if ($ch->nodeName eq 'kap') {
                    $ch->insertBefore($ofc,$ch->firstChild);
                    $inserted = 1;
                    $modified = 1;

                    last;
                }                
            } # for
        } # else
    } # if $el

    return $modified;
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
    $el->appendText = $OA;
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
        # se la ido estas tildo, ni anstataŭigu per la koncerna radiko / variaĵo
        if ($ch->nodeName eq 'tld') {            
            print "\n".$radikoj->{var_key($ch)}."\n" if ($debug); 
            $res .= $radikoj->{var_key($ch)}
        # se temas pri variaĵo ni rikure vokas extract_kap por trakti ĝin
        } elsif ($ch->nodeName eq 'var') {
            my $var = extract_kap($ch);
            # registru la derivaĵon ($node) sub la nomo $var
            $drvmap{$var} = $node;
        # tekstojn kaj literunuojn ni kolektas kiel tekstenhavo
        } elsif ($ch->nodeType eq XML_TEXT_NODE || $ch->nodeType eq XML_ENTITY_REF_NODE) {
            print $ch."\n" if ($debug);
            my $cnt = $ch->textContent();
            $cnt =~ s/,//g;
            $res .= $cnt;
        } else {
            print "NT: ".$ch->nodeType."\n" if ($debug && $ch->nodeType ne XML_ELEMENT_NODE);
        }
    };
    # registru la derivaĵon ($node) sub la kapvorto $res
    $res =~ s/^\s+|\s+$//sg;
    $drvmap{$res} = $node if ($node->nodeName() eq 'drv');
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

    return @vortoj;
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