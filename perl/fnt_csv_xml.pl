#!/usr/bin/perl

# (c) 2025 ĉe Wolfram Diestel
# laŭ GPL 2.0
#
# enmetas la fontojn el tekstdosiero (CSV: radiko;fonto;dosiero)
# en la artikolojn, la artikoloj estu en la aktuala dosierujo
#
# donu CSV-dosieron kiel unua argumento
# donu artikolojn adaptendajn en la cetero (uzante ĵokerojn):
#
# perl fnt_csv_xml.pl vrt/revo_fnt_korektoj.csv a*.xml

use XML::LibXML;
# https://metacpan.org/pod/XML::LibXML
use Text::CSV qw( csv );

use utf8;
binmode(STDOUT, "encoding(UTF-8)");

my $debug = 1;

unless ($#ARGV>=1) {
    #print($#ARGV);
    print "\n=> Certigu, ke vi troviĝas en la dosierujo kie enestas la artikoloj al kiuj\n";
    print "vi volas aldoni tradukojn el CSV-dosiero. Poste voku tiel:\n";
    print "   perl fnt_csv_xml.pl <csv-dosiero> <art>*.xml...\n\n" ;
    exit 1;
}

my $csvfile = shift @ARGV;
my @artikoloj = @ARGV;
 
#$artikolo = 'tmp/abel.xml';
#$artout = $artikolo.".out";
#%tradukoj = (
#    'abelo' => 'ee-ape, ee-abeille',
#    'abelujo' => 'ee-apur, ee-apora',
#    'abelreĝino' => 'ee-rein'
#);

my $fontoj = read_csv($csvfile);
#dump_fontoj() if ($debug);

my $dosiero;
my %radikoj;
my %kapmap;
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
    %kapmap = ();

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
    for my $kap ($doc->findnodes('.//art/kap')) {
        extract_kap($kap);
        #$radikoj->{var_key($rad)} = $rad->textContent();
        #print var_key($rad).": ".$rad->textContent()."\n" if ($debug);
    }
        
    print "kap: ".join(';',keys(%kapmap))."\n" if ($debug);

    # Nun ni scias la elementojn, kiuj povus akcepti fontojn 
    # Ni trairu ilin kaj se ili ankoraŭ ne havas la ĝustan laŭ la CSV-dosiero, ni ŝanĝos tiujn

    # Laŭ kapvortoj ni rigardu ĉu estas tradukoj por tiuj kaj se jes ni iru al drv
    # kaj provos aldoni la tradukojn inter la aliaj lingvoj laŭalfabete
    for my $rad (keys(%kapmap)) {
        print "rad: |$rad|...\n" if ($debug);
        my $f = $kapmap{$rad};
        if ($f->[1]) {
            my $mod = aldonu_fnt_al($rad,$f->[0],$f->[1]);
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

sub aldonu_fnt_al {
    # mrk/kap, tradukoj
    my ($rad,$kap,$fnt) = @_;

    my $novfnt = $fontoj->{$dosiero}->{$rad};
    my $modified = 0;

    if ($fnt) {
        $modified = replace_fnt($fnt,$novfnt);
    } else {
        $modified = insert_fnt($kap,$novfnt);
    }

    return $modified;
}

sub replace_fnt {
    ($fnt,$nov) = @_;

    $tnov = XML::LibXML::Text->new($nov);

    for my $ch ($fnt->childNodes()) {
        if ($ch->nodeName eq 'bib') {                    
            $bib = make_el('bib');
            $bib->appendText($tnov);
            $fnt->replaceChild($bib,$ch);
            return 1;
        } elsif ($ch->nodeType eq XML_TEXT_NODE && $ch->textContent() eq 'Z') {
            $fnt->replaceChild($tnov,$ch);
            return 1;
        }
    }
}

sub insert_fnt {
    ($knode,$nov) = @_;

    $fnt = make_el('fnt');
    $bib = make_el('bib');
    $bib->appendText($nov);
    $fnt->appendChilde($bib);

    $knode->appendChild($fnt);
    return 1;
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

# ekstraktu la enhavon de artikola kapvorto (inkl. de variaĵo)
# t.e. la bezonatajn rad, fnt

sub extract_kap {
    my $kap = shift;
    my $rad,$res = '';
    my @fnt = ();

    print "kap: ".$kap if ($debug);

    for my $ch ($kap->childNodes()) {
        # se temas pri variaĵo ni rikure vokas extract_kap por trakti ĝin
        #if ($ch->nodeName eq 'var') {
        #    my $var = extract_kap($ch);
        #    # registru la derivaĵon ($node) sub la nomo $var
        #    $drvmap{$var} = $node;
        ## tekstojn kaj literunuojn ni kolektas kiel tekstenhavo
        #} els
        if ($ch->nodeName eq 'rad') {
            $rad = extract_text($ch);
        #    # registru la derivaĵon ($node) sub la nomo $var
        #    $drvmap{$var} = $node;
        ## tekstojn kaj literunuojn ni kolektas kiel tekstenhavo
        } elsif ($ch->nodeName eq 'fnt') {
            #my $f = extract_fnt($ch);
            push(@fnt,$ch);
        } else {
            print "NT: ".$ch->nodeType."\n" if ($debug && $ch->nodeType ne XML_ELEMENT_NODE);
        }
    };
    # registru la fontojn ($node) sub la radiko
    if ($rad) {
        $kapmap{$rad} = [$kap,@fnt];
    }
}

sub extract_text {
    my $node = shift;
    my $res = '';
    for my $ch ($node->childNodes()) {
        # tekstojn kaj literunuojn ni kolektas kiel tekstenhavo
        if ($ch->nodeType eq XML_TEXT_NODE || $ch->nodeType eq XML_ENTITY_REF_NODE) {
            print $ch."\n" if ($debug);
            $res .= $ch->textContent();
        } else {
            print "NT: ".$ch->nodeType."\n" if ($debug && $ch->nodeType ne XML_ELEMENT_NODE);
        }
    }
    return $res
}        


sub read_csv {
	my ($csvfile) = @_;
    my $parser = Text::CSV->new ({ auto_diag => 1, sep_char => ";" });
    
    open $CSV,"<:encoding(utf8)",$csvfile or die "Ne povis malfermi CSV '$csvfile': $!\n";

    my $recs = $parser->getline_all($CSV);
    close $CSV;

    my $fontoj;

    # aŭ ni havas tri kolumnojn <eo>;<mrk>;<trd> aŭ ni havas nur eo>;<trd>
    my $kolumnoj = (scalar @{$recs->[0]});
    print "# kolumnoj en CSV: $kolumnoj\n" if ($debug);

    for $r (@$recs) {
        my $rad, $fnt, $dos;
        # se ni havas markon en la dua kolumno ni uzos tiun kiel indekso
        # aliokaze ni uzas la kapvorton en la unua kolumno kiel indekso
        $rad = $r->[0];
        $fnt = $r->[1];
        $dos = $r->[2];

        # povas esti unu aŭ pluraj tradukoj
        $fontoj->{$dos}->{$rad} = $fnt
    }

    return $fontoj;
}

sub dump_fontoj {
    for $k (keys %{$fontoj}) {
        print "$k: ".join(',',$fontoj->{$k})."\n";
    }
}

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