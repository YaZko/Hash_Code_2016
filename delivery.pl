#!/usr/bin/env perl

use strict;
use warnings;
use v5.18;
use open qw(:std :utf8);
use utf8;
use File::Basename;
use Data::Dumper;
use POSIX qw(ceil);
use List::Util qw(first all reduce sum min);

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

# Preprocessings of input
my $DiagLength = sqrt(sqr($Rows) + sqr($Cols));
my $Heart      = orders_heart();
my $WHeart     = warehouse_heart();
say "@$Heart @$WHeart $Cols $Rows ", "@{ $Wpos[0] }";
my $MeanOrderScore = sum(map { order_base_score($_) } 0 .. $#Opos) / @Opos;
my @Orders =
  map  { $_->[0] }
  sort { $a->[1] <=> $b->[1] }
  map  { [ $_, order_score($_, 7, 2) ] } 0 .. $#Onbitems;
my @Orders_after =
  map  { $_->[0] }
  sort { $a->[1] <=> $b->[1] }
  map  { [ $_, order_score($_, 0, 4) ] } 0 .. $#Onbitems;

# Main Loop
my $Count = 0;
while (defined(my $order = select_order())) {
    do_order($order) or last;
    $Count++;
    @Orders = @Orders_after if $Count == 19;
}
say scalar(@Orders);
die "Not done all orders" unless @Orders = @Odone;
check_times();
write_commands();

my $Score;
$Score += ceil(100 * (($Turns - $_) / $Turns)) for @Odone;
say "SCORE:$Score";
say scalar(@Ostock) * 100;

sub orders_heart    { pos_heart(\@Opos); }
sub warehouse_heart { pos_heart(\@Wpos); }

sub pos_heart {
    my $positions = shift;
    my $heart =
      reduce { [ $a->[0] + $b->[0], $a->[1] + $b->[1] ] } @$positions;
    my @heart = map { $_ / @$positions } @$heart;
    my @near_to_heart =
      grep {
              abs($_->[0] - $heart[0]) < $Rows / 2
          and abs($_->[1] - $heart[1]) < $Cols / 2
      } @$positions;

    # Better new heart
    $heart =
      reduce { [ $a->[0] + $b->[0], $a->[1] + $b->[1] ] } @$positions;
    @heart = map { $_ / @$positions } @$heart;
    return \@heart;
}

sub check_times {
    all { $DT[$_] <= $Turns } 0 .. $#DT or die "check_times";
}

sub check_order_done {
    my $o = shift;
    return ($Onbitems[$o] == 0);
}

sub order_base_score {
    my $o = shift;
    my $score;
    foreach my $t (keys %{ $Ostock[$o] }) {
        $score += $Ostock[$o]{$t} * $PTW[$t];
    }
    return $score;    # less is better
}

sub order_score {
    my ($o, $start_weight, $heart_weight) = @_;
    my $score = order_base_score($o);
    my $turns_to_heart = turns_between_positions($Opos[$o], $Heart);
    my $turns_to_start_pos =
      turns_between_positions($Opos[$o], $Wpos[0]);
    my $turns_to_wheart = turns_between_positions($Opos[$o], $WHeart);
    $score +=
      $MeanOrderScore *
      ($heart_weight * $turns_to_heart / $DiagLength +
          $start_weight * $turns_to_start_pos / $DiagLength +
          1 * $turns_to_wheart / $DiagLength);
    return $score;    # less is better
}

sub select_order {
    my $order = first { $Onbitems[$_] > 0 } @Orders;
    say "No more orders!" unless defined $order;
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
      sort keys %{ $Ostock[$o] };
    foreach my $t (@otypes) {
        next unless $Ostock[$o]{$t} > 0;
        my $q = $Ostock[$o]{$t};
        my ($w, $qw) = find_warehouse_where_load($t, $q, $o);
        my ($d, $qd) = find_drone_for_order($t, $qw, $o, $w);
        say "Too much turns" and return 0 unless defined $d;
        die "do_order:Weight problem" if $d == -1;
        die "do_order:No warehouse found" unless defined $w;
        die "do_order:qd $qd <= 0" unless $qd > 0;

        my @loads;
        my $first_loads = salvage_warehouse(\@otypes, $o, $d, $w);
        push @loads, @$first_loads;
        my $count = 0;
        while (not check_order_done($o) and $DW[$d] < $MaxPayload / 2.2) {
            $count++;
            last if $count > 4;
            my $tt = first { $Ostock[$o]{$_} > 0 } @otypes;
            ($w, $qw) =
              find_better_warehouse_where_load($tt, $Ostock[$o]{$tt}, $o, $d);
            my $newloads = salvage_warehouse(\@otypes, $o, $d, $w);
            last unless @$newloads;
            push @loads, @$newloads;
        }
        $DT[$d] += turns_between_positions($DP[$d], $Opos[$o]);
        $DP[$d] = $Opos[$o];
        foreach my $load (@loads) {
            my ($q, $t) = @$load;
            print Dumper(\@loads) unless defined $t;
            deliver($d, $q, $t, $o);
        }
        if (check_order_done($o)) {
            push @Odone, $DT[$d];
        }
    }
    return 1;
}

sub salvage_warehouse {
    my ($otypes, $o, $d, $w) = @_;
    $DT[$d] += turns_between_positions($DP[$d], $Wpos[$w]);
    my @loads;
    TYPE: foreach my $t (@$otypes) {
        my $q = $Ostock[$o]{$t};
        while ($q > 0) {
            if (    $Wstock[$w]{$t} >= $q
                and $q * $PTW[$t] + $DW[$d] <= $MaxPayload)
            {
                push @loads, [ $q, $t ];
                load($d, $q, $t, $w, $o);
                next TYPE;
            }
            $q--;
        }
    }
    return \@loads;
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
    my ($d) = sort { $DT[$a] <=> $DT[$b] } @light_drones;
    my $qw;
    ($w, $qw) = find_better_warehouse_where_load($t, $q, $o, $d);
    $qd = min($qd, $qw);

    return ($d, $qd);
}

sub find_warehouse_where_load {
    my ($t, $q, $o) = @_;
    my @warehouses =
      map  { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      map  { [ $_, turns_between_positions($Wpos[$_], $Opos[$o]) ] }
      0 .. $#Wstock;
    pick_warehouse_to_load(\@warehouses, $t, $q);
}

sub find_better_warehouse_where_load {
    my ($t, $q, $o, $d) = @_;
    my @warehouses =
      map  { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      map {
        [
            $_,
            turns_between_positions($Wpos[$_], $Opos[$o]) +
              turns_between_positions($DP[$d], $Wpos[$_])
        ]
      } 0 .. $#Wstock;
    pick_warehouse_to_load(\@warehouses, $t, $q);
}

sub pick_warehouse_to_load {
    my ($warehouses, $t, $q) = @_;
    my $qw;
    foreach my $qq (reverse 1 .. $q) {
        foreach my $ww (@$warehouses) {
            if ($Wstock[$ww]{$t} and $Wstock[$ww]{$t} >= $qq) {
                $qw = $qq;
                return ($ww, $qw);
            }
        }
    }
    die "pick_warehouse_to_load:no warehouse found:$t,$q\n";
}

sub check_in_initial_warehouse {
    my ($t, $q) = @_;
    my $qw;
    foreach my $qq (reverse 1 .. $q) {
        if ($Wstock[0]{$t} and $Wstock[0]{$t} >= $qq) {
            $qw = $qq;
            return $qw;
        }
    }
    return 0;
}

sub check_weight {
    my $d = shift;
    return ($DW[$d] <= $MaxPayload && $DW[$d] >= 0);
}

sub load {
    my ($d, $q, $t, $w, $o) = @_;
    push @CMDS, [ $d, "L", $w, $t, $q ];
    $DI[$d]{$t} += $q;
    $DW[$d] += $q * $PTW[$t];
    die "load:$d,$w,$t,$q:too much load" unless check_weight($d);
    $Wstock[$w]{$t} -= $q;
    die "load:$d,$w,$t,$q:no stock" if $Wstock[$w]{$t} < 0;
    $DT[$d] += 1;
    $DP[$d] = $Wpos[$w];
    $Ostock[$o]{$t} -= $q;
    $Onbitems[$o]   -= $q;
    die "deliver:$d,$o,$t,$q:order was less than that" if $Onbitems[$o] < 0;
}

sub deliver {
    my ($d, $q, $t, $o) = @_;
    push @CMDS, [ $d, "D", $o, $t, $q ];
    $DI[$d]{$t} -= $q;
    $DW[$d] -= $q * $PTW[$t];
    $DT[$d] += 1;
    die "deliver:$d,$o,$t,$q:not enough in inventory for that"
      if $DI[$d]{$t} < 0;
    die "deliver:$d,$o,$t,$q:weight problem" unless check_weight($d);
}

sub turns_between_positions {
    my ($p1, $p2) = @_;
    return ceil(sqrt(sqr($p1->[0] - $p2->[0]) + sqr($p1->[1] - $p2->[1])));
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
