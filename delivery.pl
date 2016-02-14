#!/usr/bin/env perl

use strict;
use warnings;
use v5.18;
use open qw(:std :utf8);
use utf8;
use File::Basename;
use Data::Dumper;
use POSIX qw(ceil);
use List::Util qw(first all reduce sum min shuffle max);
use GD;

# Parse stuff
my ($Rows, $Cols, $Drones, $Turns, $MaxPayload);
my @PTW;    # Product Type Weight
my (@Wpos, @Wstock, @Opos, @Onbitems, @Ostock);

my $File = $ARGV[0] // 'inputs/redundancy.in';
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
my %Odone;                 # whether order is done

# Preprocessings of input
my @CWs;                   # Closest Warehouses
foreach my $o (0 .. $#Opos) {
    $CWs[$o] = closest_warehouses($o);
}

say "ColsxRows: ${Cols}x$Rows ";
say "Start: @{ $Wpos[0] }";
my @Orders;
if ($File =~ /mother_of_all_warehouses/) {
    @Orders = compute_generic_order(\&order_score_mother);
}
else {
    @Orders = compute_generic_order(\&order_score_generic);
}

# Main Loop
my $Count = 0;
while (defined(my $order = select_order())) {
    do_order($order);
    $Count++;
    @Orders = compute_generic_order(\&order_score_generic)
      if $Count % 30 == 0 and $File =~ /redundancy/;
    @Orders = compute_generic_order(\&order_score_generic)
      if $Count % 30 == 0 and $File =~ /busy_day/;
}
say "ORDERS:", scalar(@Orders);
die "Not done all orders" unless @Orders == @Odone;
check_times();
write_commands();

my $Score;
$Score += ceil(100 * (($Turns - $_) / $Turns)) for @Odone;
say "SCORE:$Score";
say scalar(@Ostock) * 100;

# Draw map
draw_map();

exit 0;

sub compute_generic_order {
    my $sub = shift;
    map    { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      map  { [ $_, $sub->($_) ] } 0 .. $#Onbitems;
}

sub check_times {
    all { $DT[$_] <= $Turns } 0 .. $#DT or die "check_times";
}

sub check_order_done {
    my $o = shift;
    return ($Onbitems[$o] == 0);
}

sub order_score_generic {
    my $o           = shift;
    my $wc          = $CWs[$o];
    my @drone_dists = map { turns_between_positions($DP[$_], $Wpos[ $wc->[0] ]) }
      0 .. $Drones - 1;
    @drone_dists = (sort @drone_dists)[0..3] if $File =~ /busy_day/;
    my $drone_dists_mean = mean(\@drone_dists);
    my @turns_to_w =
      map { turns_between_positions($Wpos[$_], $Opos[$o]) } 0 .. $#Wpos;
    my $dist_turns = mean(\@turns_to_w);
    my $weight     = order_weight($o);
    my %t_scores;

    foreach my $t (keys %{ $Ostock[$o] }) {
        $t_scores{$t} = ceil(($Ostock[$o]{$t} * $PTW[$t]) / $MaxPayload);
    }
    my $types_count = keys %t_scores;
    my $drones_approx;
    $drones_approx = ceil($weight / $MaxPayload) * log(4 + $types_count);
    $drones_approx = ceil($weight / $MaxPayload) * log(3.9 + $types_count)
      if $File =~ /redundancy/;

    # Some "random" magic in numbers :)
    return $drones_approx * (3 * $dist_turns / 2.3 + $drone_dists_mean / 1.5) +
      2 * sum(values %t_scores)
      if $File =~ /redundancy/;
    return $drones_approx * (3 * $dist_turns / 2 + $drone_dists_mean / 2) +
      2 * sum(values %t_scores);
}

sub closest_warehouses {
    my $o = shift;
    my @dists =
      map { turns_between_positions($Wpos[$_], $Opos[$o]) } 0 .. $#Wpos;
    my @warehouses =
      sort {
        turns_between_positions($Wpos[$a], $Opos[$o])
          <=> turns_between_positions($Wpos[$b], $Opos[$o])
      } 0 .. $#Wpos;
    return \@warehouses;
}

sub order_score_mother {
    my $o          = shift;
    my $dist_turns = turns_between_positions($Wpos[0], $Opos[$o]);
    my $weight     = order_weight($o);
    my %t_scores;
    foreach my $t (keys %{ $Ostock[$o] }) {
        $t_scores{$t} = ceil(($Ostock[$o]{$t} * $PTW[$t]) / $MaxPayload);
    }
    my $types_count   = keys %t_scores;
    my $drones_approx = ceil($weight / $MaxPayload);
    return 2 * $drones_approx * $dist_turns + 2 * sum(values %t_scores);
}

sub order_weight {
    my $o = shift;
    my $weight;
    foreach my $t (keys %{ $Ostock[$o] }) {
        $weight += $Ostock[$o]{$t} * $PTW[$t];
    }
    return $weight;
}

sub select_order {
    my $order = first { $Onbitems[$_] > 0 } @Orders;
    say "No more orders!" unless defined $order;
    return $order;
}

sub type_score {
    my ($o, $t) = @_;
    return 1 / ($Ostock[$o]{$t} * $PTW[$t])
      if $File =~ /mother_of_all_warehouses/;
    return 1 / ($Ostock[$o]{$t} * $PTW[$t]) if $File =~ /redundancy/;
    return $Ostock[$o]{$t} * $PTW[$t];
}

sub max_of_type {
    my ($o, $t) = @_;
    my $n = $Ostock[$o]{$t};
    my $q = int($MaxPayload / $PTW[$t]);
    return min($n, $q) * $PTW[$t];
}

sub do_order {
    my $o = shift;
    my @otypes =
      sort { type_score($o, $a) <=> type_score($o, $b) }
      sort { $PTW[$a] <=> $PTW[$b] }
      grep { $Ostock[$o]{$_} > 0 and $PTW[$_] > 0 }
      sort keys %{ $Ostock[$o] };
    while (1) {
        my $t = first { $Ostock[$o]{$_} > 0 } @otypes;
        last unless defined $t;
        my $q = $Ostock[$o]{$t};
        my ($w, $qw) = find_warehouse_where_load($t, $q, $o);
        my $d;
        ($d, $w) = find_drone_for_order($t, $qw, $o, $w);
        die "do_order:No warehouse found" unless defined $w;

        my @loads;
        my $first_loads = salvage_warehouse(\@otypes, $o, $d, $w);
        push @loads, @$first_loads;
        die "No first loads" unless @loads;
        if ($DW[$d] < $MaxPayload) {

            # secundary orders for $d
            my $factor = 3;
            $factor = 2 if $File =~ /mother_of_all_warehouses/;
            $factor = 3 if $File =~ /redundancy/;
            my $radius_factor = 5.5;
            $radius_factor = 3 if $File =~ /mother_of_all_warehouses/;
            $radius_factor = 2 if $File =~ /redundancy/;
            my @orders = grep {
                $Onbitems[$_] > 0
                and $_ != $o
                and turns_between_positions($DP[$d], $Opos[$_])
                  + turns_between_positions($Opos[$_], $Wpos[$w])
                  < $factor * turns_between_positions($Opos[$o], $Wpos[$w])
                and turns_between_positions($Opos[$_], $Opos[$o])
                  < turns_between_positions($Opos[$o], $Wpos[$w]) / $radius_factor
            } @Orders;
            my $payload_factor = 5 / 6;
            $payload_factor = 4 / 5 if $File =~ /mother_of_all_warehouses/;
            $payload_factor = 4 / 5 if $File =~ /redundancy/;

            for my $o2 (@orders) {
                if ($DW[$d] < $payload_factor * $MaxPayload) {
                    my @otypes =
                      sort { type_score($o2, $a) <=> type_score($o2, $b) }
                      sort { $PTW[$a] <=> $PTW[$b] }
                      grep { $Ostock[$o2]{$_} > 0 and $PTW[$_] > 0 }
                      sort keys %{ $Ostock[$o2] };
                    my $newloads = salvage_warehouse(\@otypes, $o2, $d, $w);
                    push @loads, @$newloads if @$newloads;
                }
            }
        }
        foreach my $load (@loads) {
            my ($q, $t, $o) = @$load;
            deliver($d, $q, $t, $o);
            if (check_order_done($o) and not $Odone{$o}) {
                push @Odone, $DT[$d] - 1;
                $Odone{$o}++;
            }
        }
    }
    return 1;
}

sub salvage_warehouse {
    my ($otypes, $o, $d, $w) = @_;
    my @loads;
    TYPE: foreach my $t (@$otypes) {
        my $q = $Ostock[$o]{$t};
        while ($q > 0) {
            if (    $Wstock[$w]{$t} >= $q
                and $q * $PTW[$t] + $DW[$d] <= $MaxPayload)
            {
                push @loads, [ $q, $t, $o ];
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
        die "No usable drones…";
    }
    my $d;
    if ($File =~ /mother_of_all_warehouses/) {
        ($d) = sort { $DT[$a] <=> $DT[$b] } @usable_drones;
    }
    else {
        ($d) = sort {
            $DT[$a] +
              turns_between_positions($DP[$a], $Wpos[$w]) <=> $DT[$b] +
              turns_between_positions($DP[$b], $Wpos[$w])
        } @usable_drones;
    }
    $w = find_better_warehouse_where_load($t, $q, $o, $d);
    return ($d, $w);
}

sub warehouse_suitability_for_order {
    my ($w, $o) = @_;
    my $weight;
    foreach my $t (keys %{ $Ostock[$o] }) {
        $weight += min($Wstock[$w]{$t}, $Ostock[$o]{$t}) * $PTW[$t];
    }
    my $suitable = $weight / $MaxPayload > 1 ? 1 : $weight / $MaxPayload;
    return 2 - $suitable;
}

sub find_warehouse_where_load {
    my ($t, $q, $o) = @_;
    my @warehouses =
      map  { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      map {
        [ $_,
            turns_between_positions($Wpos[$_], $Opos[$o]) *
              warehouse_suitability_for_order($_, $o) ]
      } 0 .. $#Wstock;
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
            (
                turns_between_positions($Wpos[$_], $Opos[$o]) +
                  turns_between_positions($DP[$d], $Wpos[$_])
            ) * warehouse_suitability_for_order($_, $o)
        ]
      } 0 .. $#Wstock;
    my ($w) = pick_warehouse_to_load(\@warehouses, $t, $q);
    return $w;
}

sub pick_warehouse_to_load {
    my ($warehouses, $t, $q) = @_;
    my $qw;
    while ($q > 0) {
        foreach my $ww (@$warehouses) {
            if ($Wstock[$ww]{$t} and $Wstock[$ww]{$t} >= $q) {
                $qw = $q;
                return ($ww, $qw);
            }
        }
        $q--;
    }
    die "pick_warehouse_to_load:no warehouse found:$t\n";
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
    $DT[$d] += turns_between_positions($DP[$d], $Wpos[$w]);
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
    $DT[$d] += turns_between_positions($DP[$d], $Opos[$o]);
    $DP[$d] = $Opos[$o];
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

sub std_dev {
    my $n   = @{ $_[0] };
    my $sum = sum(@{ $_[0] });
    $sum ? sqrt((sum(map sqr($_), @{ $_[0] }) - sqr($sum) / $n) / $n) : -1;
}

sub mean {
    my $nums = shift;
    return 0 unless @$nums;
    my $sum = sum(@$nums);
    return $sum / @$nums;
}

sub draw_map {
    my $im = GD::Image->new($Rows * 3, $Cols * 3);
    my $white = $im->colorAllocate(255, 255, 255);
    my $black = $im->colorAllocate(0,   0,   0);
    my $red   = $im->colorAllocate(255, 0,   0);
    my $blue  = $im->colorAllocate(0,   0,   255);
    my $green = $im->colorAllocate(0,   255, 0);
    $im->rectangle(0, 0, $Rows * 3, $Cols * 3, $white);
    my $count = 0;

    foreach my $pos (@Opos) {
        my ($x, $y) = map { $_ * 3 } @$pos;
        $im->filledEllipse($x, $y, 5, 5, $blue);
        $count++;
    }
    foreach my $o (@Orders[ 0 .. 300 ]) {
        my $pos = $Opos[$o];
        my ($x, $y) = map { $_ * 3 } @$pos;
        $im->filledEllipse($x, $y, 10, 10, $blue);
    }
    foreach my $pos (@Wpos) {
        my ($x, $y) = map { $_ * 3 } @$pos;
        $im->filledEllipse($x, $y, 15, 15, $red);
    }
    $im->filledEllipse($Wpos[0]->[0] * 3, $Wpos[0]->[1] * 3, 30, 30, $green);
    open(my $fh, '>', 'map.png') or die "draw_map:$!";
    binmode $fh;
    print $fh $im->png;
    close $fh;
}
