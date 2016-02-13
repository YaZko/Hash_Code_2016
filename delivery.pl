#!/usr/bin/env perl

use strict;
use warnings;
use v5.18;
use open qw(:std :utf8);
use utf8;
use File::Basename;
use POSIX qw(ceil);
use List::Util qw(first all);

# Parse stuff
my ($Rows, $Cols, $Drones, $Turns, $MaxPayload);
my @PTW;    # Product Type Weight
my (@Wpos, @Wstock, @Opos, @Onbitems, @Ostock);

my $File = $ARGV[0] // 'inputs/busy_day.in';
parse($File);

my @DT = (0) x $Drones;    # Drone State
my @DP;                    # Drone Position
my @DI;                    # Drone Inventory
my @DW = (0) x $Drones;    # Drone Weight
for (0 .. $Drones - 1) {
    $DP[$_] = $Wpos[0];
}

my @CMDS;                  # Commands to be output
my @Odone;                 # times of orders completion

# Main Loop
while (defined(my $order = select_order())) {
    do_order($order) or last;
}
check_times();
write_commands();

my $Score;
$Score += ceil(100 * (($Turns - $_) / $Turns)) for @Odone;
say "SCORE:$Score";
say scalar(@Ostock) * 100;

sub check_times {
    all { $DT[$_] <= $Turns } 0 .. $#DT or die "check_times";
}

sub check_order_done {
    my $o = shift;
    my $s = 0;
    $s += $Ostock[$o]{$_} for keys %{ $Ostock[$o] };
    return ($s == 0);
}

sub order_score {
    my $o = shift;
    my $score;
    foreach my $t (keys %{ $Ostock[$o] }) {
        $score += $Ostock[$o]{$t} * 2 * ($PTW[$t] / $MaxPayload);
    }
    my $turns_to_center =
      turns_between_positions($Opos[$o], [ $Rows / 2, $Cols / 2 ]);
    $score +=
      ($Rows + $Cols) / 50 * $turns_to_center / sqrt(sqr($Rows) + sqr($Cols));
    return $score;    # less is better
}

sub select_order {
    my @orders =
      map  { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      map  { [ $_, order_score($_) ] } 0 .. $#Onbitems;
    my $order = first { $Onbitems[$_] > 0 } @orders;
    say "No more orders…" unless defined $order;
    return $order;
}

sub type_score {
    my ($o, $t) = @_;
    $Ostock[$o]{$t} * $PTW[$t];
}

sub do_order {
    my $o = shift;
    my @otypes =
      sort { type_score($o, $a) <=> type_score($o, $b) }
      keys %{ $Ostock[$o] };
    foreach my $t (@otypes) {
        next unless $Ostock[$o]{$t} > 0;
        my $q = $Ostock[$o]{$t};
        my ($w, $qw) = find_warehouse_where_load($t, $q, $o);
        my ($d, $qd) = find_drone_for_order($t, $qw, $o, $w);
        unless (defined $d) {
            say "Too much turns";
            return 0;
        }
        if ($d == -1) {
            say "Weight problem";
            return 0;
        }
        unless (defined $w) {
            say "No warehouse found";
            return 0;
        }
        load($d, $qd, $t, $w);
        my @loads;
        foreach my $tt (@otypes) {
            next if $tt == $t;
            my $qq = $Ostock[$o]{$tt};
            if (    $qq > 0
                and $Wstock[$w]{$tt} > $qq
                and $qq * $PTW[$tt] + $DW[$d] < $MaxPayload)
            {
                push @loads, [ $qq, $tt ];
                load($d, $qq, $tt, $w);
            }
        }
        deliver($d, $qd, $t, $o);
        foreach my $load (@loads) {
            my ($qq, $tt) = @$load;
            deliver($d, $qq, $tt, $o);
        }
        if (check_order_done($o)) {
            push @Odone, $DT[$d];
        }
    }
    return 1;
}

sub find_drone_for_order {
    my ($t, $q, $o, $w) = @_;
    my @usable_drones = grep {
        $DT[$_] +
          turns_between_positions($DP[$_], $Wpos[$w]) +
          turns_between_positions($DP[$_], $Opos[$o]) <= $Turns
    } 0 .. $#DT;
    unless (@usable_drones) {
        say "No usable drones…";
        return;
    }
    my @light_drones;
    my $qd;
    foreach my $qq (reverse 1 .. $q) {
        @light_drones =
          grep { $DW[$_] + $qq * $PTW[$t] <= $MaxPayload } @usable_drones;
        if (@light_drones) {
            $qd = $qq;
            last;
        }
    }
    unless (@light_drones) {
        return -1;
    }
    my ($dd) = sort { $DT[$a] <=> $DT[$b] } @light_drones;
    $DT[$dd] += turns_between_positions($DP[$dd], $Wpos[$w]);
    $DT[$dd] += turns_between_positions($DP[$dd], $Opos[$o]);
    return ($dd, $qd);
}

sub find_warehouse_where_load {
    my ($t, $q, $o) = @_;
    my $w;
    my @warehouses =
      map  { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      map  { [ $_, turns_between_positions($Wpos[$_], $Opos[$o]) ] }
      0 .. $#Wstock;
    my $qw;
    foreach my $qq (reverse 1 .. $q) {
        foreach my $ww (@warehouses) {
            if ($Wstock[$ww]{$t} and $Wstock[$ww]{$t} >= $qq) {
                $qw = $qq;
                return ($ww, $qw);
            }
        }
    }
    die "find_warehouse_where_load:no warehouse found:$t,$q\n";
}

sub check_weight {
    my $d = shift;
    return ($DW[$d] <= $MaxPayload && $DW[$d] >= 0);
}

sub load {
    my ($d, $q, $t, $w) = @_;
    push @CMDS, [ $d, "L", $w, $t, $q ];
    $DI[$d]{$t} += $q;
    $DW[$d] += $q * $PTW[$t];
    die "load:$d,$w,$t,$q:too much load" unless check_weight($d);
    $Wstock[$w]{$t} -= $q;
    die "load:$d,$w,$t,$q:no stock" if $Wstock[$w]{$t} < 0;
    $DP[$d] = $Wpos[$w];
}

sub deliver {
    my ($d, $q, $t, $o) = @_;
    push @CMDS, [ $d, "D", $o, $t, $q ];
    $DI[$d]{$t} -= $q;
    $DW[$d] -= $q * $PTW[$t];
    die "deliver:$d,$o,$t,$q:not enough in inventory for that"
      if $DI[$d]{$t} < 0;
    die "deliver:$d,$o,$t,$q:weight problem" unless check_weight($d);
    $Ostock[$o]{$t} -= $q;
    $Onbitems[$o]   -= $q;
    die "deliver:$d,$o,$t,$q:order was less than that" if $Onbitems[$o] < 0;
    $DP[$d] = $Opos[$o];
}

sub turns_between_positions {
    my ($p1, $p2) = @_;
    return 1 + ceil(sqrt(sqr($p1->[0] - $p2->[0]) + sqr($p1->[1] - $p2->[1])));
}

sub sqr { $_[0] * $_[0] }

sub write_commands {
    my $file = basename($File);
    open(my $fh, '>', $file . "out") or die "write_commands:$file:$!";
    say $fh scalar(@CMDS);
    say $fh "@{ $_ }" foreach @CMDS;
    close $fh;
}

sub parse {
    my $file = shift;
    open(my $fh, '<', $file) or die "$file:$!";
    ($Rows, $Cols, $Drones, $Turns, $MaxPayload) = <$fh> =~ /(\d+)/g;
    my ($product_types_nb) = <$fh> =~ /(\d+)/;    # Useless
    @PTW = <$fh> =~ /(\d+)/g;
    my ($warehouses_nb) = <$fh> =~ /(\d+)/;
    for my $w (0 .. $warehouses_nb - 1) {
        my @pos = <$fh> =~ /(\d+)/g;
        $Wpos[$w] = \@pos;
        my $items = <$fh>;
        my $count = 0;
        while ($items =~ /(\d+)/g) {
            $Wstock[$w]{$count} = $1;
            $count++;
        }
    }
    my ($orders_nb) = <$fh> =~ /(\d+)/;
    for my $o (0 .. $orders_nb - 1) {
        my @pos = <$fh> =~ /(\d+)/g;
        $Opos[$o] = \@pos;
        my ($items_nb) = <$fh> =~ /(\d+)/;
        $Onbitems[$o] = $items_nb;    # Not very usefull
        my $items = <$fh>;
        chomp $items;
        while ($items =~ /(\d+)/g) {
            $Ostock[$o]{$1}++;
        }
    }
}
