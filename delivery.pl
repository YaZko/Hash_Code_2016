#!/usr/bin/env perl

use strict;
use warnings;
use v5.18;
use open qw(:std :utf8);
use utf8;
use File::Basename;
use Data::Dumper;
use Algorithm::Permute;
use POSIX qw(ceil);
use List::Util qw(first all reduce sum min shuffle max);
use GD;

# Parse stuff
my ($Rows, $Cols, $Drones, $Turns, $MaxPayload);
my @PTW;    # Product Type Weight
my (@Wpos, @Wstock, @Opos, @Onbitems, @Odemand);

my $File = $ARGV[0] // 'inputs/busy_day.in';
parse($File);

my @DT = (0) x $Drones;    # Drone State
my @DP;                    # Drone Position
my @DI;                    # Drone Inventory
my @DLO = (0) x $Drones;   # Drone Last Order
my @DW  = (0) x $Drones;   # Drone Weight
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

my @WTD;                   # Warehouse Type Demand
my @WEQ;                   # Warehouse Entry Queue

foreach my $w (0 .. $#Wpos) {
    foreach my $t (keys %{ $Wstock[$w] }) {
        $WTD[$w]{$t} -= $Wstock[$w]{$t};
    }
}
foreach my $o (0 .. $#Opos) {
    my $cw = $CWs[$o]->[0];
    foreach my $t (keys %{ $Odemand[$o] }) {
        $WTD[$cw]{$t} += $Odemand[$o]{$t};
    }
}
foreach my $w (0 .. $#Wpos) {
    foreach my $t (keys %{ $WTD[$w] }) {
        next unless $WTD[$w]{$t} > 0;
        $WEQ[$w]{$t} = [];
    }
}

say "ColsxRows: ${Cols}x$Rows ";
say "Start: @{ $Wpos[0] }";
my @Orders = 0 .. $#Opos;
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
say "ORDERS:", scalar(@Opos);
die "Not done all orders" unless @Opos == @Odone;
check_times();
write_commands();

my $Score;
$Score += ceil(100 * (($Turns - $_) / $Turns)) for @Odone;
say "SCORE:$Score";
say scalar(@Odemand) * 100;

# Draw map
draw_map();

exit 0;

sub compute_generic_order {
    my $sub = shift;
    map    { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      map  { [ $_, $sub->($_) ] }
      grep { $Onbitems[$_] > 0 } @Orders;
}

sub check_times {
    all { $DT[$_] <= $Turns } 0 .. $#DT or die "check_times";
}

sub check_order_done {
    my $o = shift;
    return ($Onbitems[$o] == 0);
}

sub order_score_generic {
    my $o  = shift;
    my $closest_warehouses = closest_warehouses($o);

    my %t_scores;
    my @weights = (0) x @Wpos;
    my %stock;
    foreach my $t (keys %{ $Odemand[$o] }) {
        $stock{$t} = $Odemand[$o]{$t}
    }
    foreach my $w (@$closest_warehouses) {
        foreach my $t (keys %{ $Odemand[$o] }) {
            my $q = min($Odemand[$o]{$t}, -$WTD[$w]{$t}, $stock{$t});
            next unless $q > 0;
            $stock{$t} -= $q;
            $weights[$w] += $q * $PTW[$t];
        }
    }
    foreach my $t (keys %{ $Odemand[$o] }) {
        $t_scores{$t} = ceil(($Odemand[$o]{$t} * $PTW[$t]) / $MaxPayload);
    }
    my $types_count = keys %t_scores;
    my @drones_approx = map { ceil($_ / $MaxPayload) * log(4 + $types_count) } @weights;
    my $turns;
    foreach my $w (@$closest_warehouses) {
        $turns += turns_between_positions($Wpos[$w], $Opos[$o]) * $drones_approx[$w]
          if $drones_approx[$w]; # don't compute turns if not necessary
    }
    $turns += 2 * sum(values %t_scores);
    return $turns;
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
    foreach my $t (keys %{ $Odemand[$o] }) {
        $t_scores{$t} = ceil(($Odemand[$o]{$t} * $PTW[$t]) / $MaxPayload);
    }
    my $types_count   = keys %t_scores;
    my $drones_approx = ceil($weight / $MaxPayload);
    return 2 * $drones_approx * $dist_turns + 2 * sum(values %t_scores);
}

sub order_weight {
    my $o = shift;
    my $weight;
    foreach my $t (keys %{ $Odemand[$o] }) {
        $weight += $Odemand[$o]{$t} * $PTW[$t];
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

    # XXX find better heuristic… Perhaps demand.
    return -($Odemand[$o]{$t} * $PTW[$t])
      if $File =~ /mother_of_all_warehouses/;
    return -($Odemand[$o]{$t} * $PTW[$t]) if $File =~ /redundancy/;

    #return $Odemand[$o]{$t} * $PTW[$t];
    return -$WTD[ $CWs[$o]->[0] ]{$t};
}

sub max_of_type {
    my ($o, $t) = @_;
    my $n = $Odemand[$o]{$t};
    my $q = int($MaxPayload / $PTW[$t]);
    return min($n, $q) * $PTW[$t];
}

sub mean_pos {
    my ($pos1, $w1, $pos2) = @_;
    [ $w1 * $pos1->[0] + (1 - $w1) * $pos2->[0],
        $w1 * $pos1->[1] + (1 - $w1) * $pos2->[1] ];
}

sub get_sorted_otypes {
    my $o = shift;
    sort { type_score($o, $a) <=> type_score($o, $b) }
      sort { $PTW[$a] <=> $PTW[$b] }
      grep { $Odemand[$o]{$_} > 0 and $PTW[$_] > 0 }
      sort keys %{ $Odemand[$o] };
}

sub find_good_intermediate_warehouse {
    my ($w, $d, $warehouses) = @_;
    my @dw_and_turns =
      sort { $a->[1] <=> $b->[1] }
      map {
        [ $_,
            turns_between_positions($DP[$d], $Wpos[$_]) +
              turns_between_positions($Wpos[$_], $Wpos[$w]) ]
      } @$warehouses;
    my ($dw, $turns) = @{ shift @dw_and_turns };
    return ($dw, $turns);
}

sub take_things_for_another_warehouse_and_unload {
    my ($d, $dw, $w) = @_;
    my $loads = take_for_other_warehouse($d, $dw, $w);
    foreach my $load (@$loads) {
        my ($q, $t) = @$load;
        unload($d, $w, $t, $q);
    }
}

sub do_order {
    my $o      = shift;
    my @otypes = get_sorted_otypes($o);
    while (1) {
        last unless $Onbitems[$o] > 0;
        my ($w, $qw) = find_warehouse_where_load($o);
        my $d;
        my $old_w = $w;
        ($d, $w) = find_drone_for_order($o, $w);
        my $waits = 0;
        while (not defined $w) {
            wait_turns($d, 5);
            ($d, $w) = find_drone_for_order($o, $old_w);
            $waits++;
            die "Too much waiting" if $waits > 1000;
        }
        if (defined $DLO[$d] and $File !~ /mother_of_all_warehouses/) {
            my $mid_factor = 0.5;
            my $middist_factor = 1.25;
            my $dist_factor = 1.20;
            my @wcandidates = grep { $_ != $w } 0 .. $#Wpos;
            my ($dw, $turns) =
              find_good_intermediate_warehouse($w, $d, \@wcandidates);
            if (turns_between_positions($DP[$d], $Wpos[$dw]) >
                0.5 * turns_between_positions($DP[$d], $Wpos[$w]))
            {
                my @dwcandidates = grep { $_ != $w and $_ != $dw } 0 .. $#Wpos;
                my ($idw, $iturns) =
                  find_good_intermediate_warehouse($dw, $d, \@dwcandidates);
                if ($iturns <
                    1.25 * turns_between_positions($DP[$d], $Wpos[$dw]))
                {
                    take_things_for_another_warehouse_and_unload($d, $idw, $dw);
                }
            }
            if ($turns < 1.20 * turns_between_positions($DP[$d], $Wpos[$w])) {
                take_things_for_another_warehouse_and_unload($d, $dw, $w);
            }
        }

        my %loads;
        my $first_loads = salvage_warehouse(\@otypes, $o, $d, $w);
        $loads{$o} = [];
        push @{ $loads{$o} }, @$first_loads;
        die "No first loads: $DW[$d]" unless @{ $loads{$o} };
        if ($DW[$d] < $MaxPayload) {

            # secundary orders for $d
            $old_w = $w if $File =~ /busy_day/;
            my $factor = 0.20;
            $factor = 0.30 if $File =~ /mother_of_all_warehouses/;
            $factor = 0.30 if $File =~ /redundancy/;
            my @orders = grep {
                      $Onbitems[$_] > 0
                  and $_ != $o
                  and
                  turns_between_positions(mean_pos($Opos[$o], 0.9, $Wpos[$w]),
                    $Opos[$_]) <
                  $factor * turns_between_positions($Opos[$o], $Wpos[$old_w])
            } @Orders;
            my $payload_factor = 5 / 6;
            $payload_factor = 4 / 5 if $File =~ /mother_of_all_warehouses/;
            $payload_factor = 4 / 5 if $File =~ /redundancy/;

            for my $o2 (@orders) {
                if ($DW[$d] < $payload_factor * $MaxPayload) {
                    my @otypes = get_sorted_otypes($o2);
                    my $newloads = salvage_warehouse(\@otypes, $o2, $d, $w);
                    next unless @$newloads;
                    $loads{$o2} = [];
                    push @{ $loads{$o2} }, @$newloads if @$newloads;
                }
            }
        }
        my @orders              = sort keys %loads;
        my @better_orders_order = @orders;
        my $wins                = compute_path_dist(\@orders, $DP[$d]);
        Algorithm::Permute::permute {
            my $new = compute_path_dist(\@orders, $DP[$d]);
            @better_orders_order = @orders if $new < $wins;
            $wins                = $new    if $new < $wins;
        }
        @orders;
        foreach my $oo (@better_orders_order) {
            my $ooloads = $loads{$oo};
            foreach my $load (@$ooloads) {
                my ($q, $t, $o) = @$load;
                deliver($d, $q, $t, $o);
                if (check_order_done($o) and not $Odone{$o}) {
                    push @Odone, $DT[$d] - 1;
                    $Odone{$o}++;
                }
            }
        }
    }
    return 1;
}

sub compute_path_dist {
    my ($orders, $init_pos) = @_;
    my $turns = turns_between_positions($init_pos, $Opos[ $orders->[0] ]);
    for (my $i = 0 ; $i < $#{$orders} ; $i++) {
        $turns += turns_between_positions($Opos[ $orders->[$i] ],
            $Opos[ $orders->[ $i + 1 ] ]);
    }
    my $last_order = $orders->[ $#{$orders} ];
    $turns += turns_between_positions($Opos[$last_order],
        $Wpos[ $CWs[$last_order]->[0] ]);
    return $turns;
}

sub salvage_warehouse {
    my ($otypes, $o, $d, $w) = @_;
    my @loads;
    my $arrival_time = $DT[$d] + turns_between_positions($DP[$d], $Wpos[$w]);
    TYPE: foreach my $t (@$otypes) {
        my $q = $Odemand[$o]{$t};
        while ($q > 0) {
            if (get_stock($w, $t, $arrival_time) >= $q
                and $q * $PTW[$t] + $DW[$d] <= $MaxPayload)
            {
                push @loads, [ $q, $t, $o ];
                load_for_order($d, $w, $t, $q, $o);
                next TYPE;
            }
            $q--;
        }
    }
    return \@loads;
}

sub take_for_other_warehouse {
    my ($d, $dw, $w) = @_;
    my $arrival_time =
      turns_between_positions($Wpos[$dw], $Wpos[$w]) +
      turns_between_positions($DP[$d],    $Wpos[$dw]);
    my @needed_types =
      map  { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      map  { [ $_, get_stock($w, $_, $DT[$d] + $arrival_time) ] }
      grep { $WTD[$w]{$_} > 0 }
      sort keys %{ $WTD[$w] };
    my @loads;
    TYPE: foreach my $t (@needed_types) {
        my $offer = -$WTD[$dw]{$t};
        next unless defined $offer and $offer > 0;
        my $q = min($offer, $WTD[$w]{$t}, $Wstock[$dw]{$t});    # XXX
        while ($q > 0) {
            if ($q * $PTW[$t] + $DW[$d] <= $MaxPayload) {
                push @loads, [ $q, $t ];
                load_for_warehouse($d, $dw, $t, $q, $w);
                next TYPE;
            }
            $q--;
        }
    }
    return \@loads;
}

sub find_drone_for_order {
    my ($o, $w) = @_;
    my @usable_drones = grep {
        $DT[$_] +
          turns_between_positions($DP[$_], $Wpos[$w]) +
          turns_between_positions($DP[$_], $Opos[$o]) <= $Turns
    } 0 .. $#DT;    # XXX This check is not totally correct, but ok in practice
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
    $w = find_better_warehouse_where_load($o, $d);
    return ($d, $w);
}

sub warehouse_suitability_for_order {
    my ($w, $o) = @_;
    warehouse_suitability_for_order_at_time($w, $o, $Turns);
}

sub warehouse_revised_suitability_for_order {
    my ($w, $d, $o) = @_;
    my $arrival_time = $DT[$d] + turns_between_positions($DP[$d], $Wpos[$w]);
    warehouse_suitability_for_order_at_time($w, $o, $arrival_time);
}

sub warehouse_suitability_for_order_at_time {
    my ($w, $o, $time) = @_;
    my $weight = compute_warehouse_weight_for_order($w, $o, $time);
    compute_suitability_from_weight($weight);
}

sub compute_suitability_from_weight {
    my $weight = shift;
    return 999 unless $weight;
    my $suitable = $weight / $MaxPayload > 1 ? 1 : $weight / $MaxPayload;
    return 2 - $suitable;
}

sub compute_warehouse_weight_for_order {
    my ($w, $o, $time) = @_;
    my $weight;
    foreach my $t (keys %{ $Odemand[$o] }) {
        $weight += min(get_stock($w, $t, $time), $Odemand[$o]{$t}) * $PTW[$t];
    }
    die "\$Onbitems[$o] == 0" unless $Onbitems[$o] > 0;
    return $weight;
}

sub find_warehouse_where_load {
    my ($o) = @_;
    my @warehouses =
      map  { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      map {
        [
            $_,
            turns_between_positions($Wpos[$_], $Opos[$o]) *
              warehouse_suitability_for_order($_, $o)
        ]
      } 0 .. $#Wstock;
    return shift @warehouses;
}

sub find_better_warehouse_where_load {
    my ($o, $d) = @_;
    my @suitabilities =
      map { warehouse_revised_suitability_for_order($_, $d, $o) } 0 .. $#Wstock;
    my @warehouses =
      map  { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      map {
        [
            $_,
            (
                turns_between_positions($Wpos[$_], $Opos[$o]) +
                  turns_between_positions($DP[$d], $Wpos[$_])
            ) * $suitabilities[$_]
        ]
      }
      grep { $suitabilities[$_] < 2 } 0 .. $#Wstock;
    return shift @warehouses;
}

sub check_weight {
    my $d = shift;
    return ($DW[$d] <= $MaxPayload && $DW[$d] >= 0);
}

sub load_for_order {
    my ($d, $w, $t, $q, $o) = @_;
    push @CMDS, [ $d, "L", $w, $t, $q ];
    $DI[$d]{$t} += $q;
    $DW[$d] += $q * $PTW[$t];
    $WTD[ $CWs[$o]->[0] ]{$t} -= $q;
    die "load:$d,$w,$t,$q:too much load" unless check_weight($d);
    $Wstock[$w]{$t} -= $q;
    $WTD[$w]{$t} += $q if $w != $CWs[$o]->[0];    # XXX
    $DT[$d] += turns_between_positions($DP[$d], $Wpos[$w]);
    die "load:$d,$w,$t,$q:no stock" if get_stock($w, $t, $DT[$d]) < 0;
    $DT[$d] += 1;
    $DP[$d] = $Wpos[$w];
    $Odemand[$o]{$t} -= $q;
    $Onbitems[$o] -= $q;
    die "deliver:$d,$o,$t,$q:order was less than that" if $Onbitems[$o] < 0;
}

sub load_for_warehouse {
    my ($d, $dw, $t, $q, $w) = @_;
    push @CMDS, [ $d, "L", $dw, $t, $q ];
    $DI[$d]{$t} += $q;
    $DW[$d] += $q * $PTW[$t];
    die "load:$d,$dw,$t,$q:too much load" unless check_weight($d);
    $Wstock[$dw]{$t} -= $q;
    $DT[$d] += turns_between_positions($DP[$d], $Wpos[$dw]);
    die "load:$d,$w,$t,$q:no stock" if get_stock($dw, $t, $DT[$d]) < 0;
    $WTD[$dw]{$t} += $q;
    die "load:$d,$w,$t,$q:wanted" if $WTD[$dw]{$t} > 0;
    $WTD[$w]{$t} -= $q;
    die "load:$d,$w,$t,$q:not wanted" if $WTD[$w]{$t} < 0;
    $DT[$d] += 1;
    $DP[$d] = $Wpos[$dw];
}

sub unload {
    my ($d, $w, $t, $q) = @_;
    push @CMDS, [ $d, "U", $w, $t, $q ];
    $DT[$d] += turns_between_positions($DP[$d], $Wpos[$w]);
    $DP[$d] = $Wpos[$w];
    $DW[$d] -= $q * $PTW[$t];
    push @{ $WEQ[$w]{$t} }, [ $q, $DT[$d] ];

    # This means that at $DT[$d] it is possible to do a load
    $DT[$d] += 1;

    # NOTE: After because unloads are done before loads
}

sub get_stock {
    my ($w, $t, $turn) = @_;
    my $stock = $Wstock[$w]{$t} // 0;
    my $stocked_unloads = 0;
    if (defined $WEQ[$w]{$t}) {
        $stocked_unloads =
          sum(map { $_->[0] } grep { $_->[1] < $turn } @{ $WEQ[$w]{$t} }) || 0;
    }
    return $stock + $stocked_unloads;
}

sub deliver {
    my ($d, $q, $t, $o) = @_;
    push @CMDS, [ $d, "D", $o, $t, $q ];
    $DI[$d]{$t} -= $q;
    $DW[$d] -= $q * $PTW[$t];
    $DT[$d] += 1;
    $DT[$d] += turns_between_positions($DP[$d], $Opos[$o]);
    $DP[$d]  = $Opos[$o];
    $DLO[$d] = $o;
    die "deliver:$d,$o,$t,$q:not enough in inventory for that"
      if $DI[$d]{$t} < 0;
    die "deliver:$d,$o,$t,$q:weight problem" unless check_weight($d);
}

sub wait_turns {
    my ($d, $turns) = @_;
    push @CMDS, [ $d, "W", $turns ];
    $DT[$d] += $turns;
    die "wait:wait too many turns:$d,$turns" if $DT[$d] > $Turns;
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
            $Odemand[$o]{$1}++;
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
    foreach my $o (sort { $Odone[$a] <=> $Odone[$b] } 0 .. 200) {
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
