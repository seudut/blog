#!/usr/bin/perl -w

use strict;

use CGI qw/:standard/;

print `cd .. && make update`;

print header;
print start_html("Update");
print br;

print "Hello world\n";

print end_html;
