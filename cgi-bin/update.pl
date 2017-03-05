#!/usr/bin/perl -w

use strict;

use CGI qw/:standard/;

my $out = `cd .. && make update 2>&1`;
my $ret = $?;

print header;
print start_html("Update");
print br;

print "Hello world\n";
print $ret;
print $out;

print end_html;
