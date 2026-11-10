#!/bin/sh
# odoc's markdown backend wraps every inline code span in single backticks,
# even when the code contains a backtick (for example ~consume:`Partial).

perl -i -pe '
  if (/^```/) { $fence = !$fence; next }
  next if $fence;
  s{(?<!`)`((?:[^`\n]|`(?=[A-Z]))+?)`(?![`A-Z])}{
    my $c = $1;
    if ($c =~ /`/) {
      my ($n) = sort { $b <=> $a } map { length } $c =~ /(`+)/g;
      my $d = "`" x ($n + 1);
      "$d $c $d";
    } else { "`$c`" }
  }ge;
' "$1"
