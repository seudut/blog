#!/usr/bin/perl -w

use strict;

use CGI qw/:standard/;

print header;
print start_html("Update");
print br;
print `make update`;

print "Hello world\n";

print end_html;
