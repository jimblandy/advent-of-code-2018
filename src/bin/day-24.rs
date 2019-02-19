#![feature(range_is_empty)]

extern crate advent_of_code_2018 as aoc;

use std::cmp::min;
use std::collections::{BTreeSet, HashSet};
use std::iter::FromIterator;

#[derive(Clone, Debug)]
struct Group {
    id: usize,
    units: usize,
    hp_per_unit: usize,
    immunities: HashSet<&'static str>,
    weaknesses: HashSet<&'static str>,
    damage_type: &'static str,
    damage: usize,
    initiative: usize,
}

impl Group {
    fn effective_power(&self) -> usize {
        self.units * self.damage
    }

    fn damage(&self, other: &Group) -> usize {
        if other.immunities.contains(&self.damage_type) {
            0
        } else if other.weaknesses.contains(&self.damage_type) {
            self.effective_power() * 2
        } else {
            self.effective_power()
        }
    }
}

macro_rules! group {
    ($units:tt units each with $hp:tt hit points $immunities_weaknesses:tt
     with an attack that does $damage:tt $damage_type:tt damage at initiative $initiative:tt) => {{
        let (immunities, weaknesses) = parse_immunities_weaknesses!($immunities_weaknesses);
        Group {
            id: 0,
            units: $units,
            hp_per_unit: $hp,
            immunities,
            weaknesses,
            damage_type: stringify!($damage_type),
            damage: $damage,
            initiative: $initiative,
        }
    }};
}

macro_rules! parse_immunities_weaknesses {
    ( ( ) ) => { (HashSet::new(), HashSet::new()) };
    ( ( immune to $( $immunity:ident ),* ) ) => {
        ( set_from_ids!($($immunity),*), HashSet::new() )
    };
    ( ( weak to $( $weakness:ident ),* ) ) => {
        ( HashSet::new(), set_from_ids!($($weakness),*) )
    };
    ( ( immune to $( $immunity:ident ),* ; weak to $( $weakness:ident ),* ) ) => {
        ( set_from_ids!($($immunity),*), set_from_ids!($($weakness),*) )
    };
    ( ( weak to $( $weakness:ident ),* ; immune to $( $immunity:ident ),* ) ) => {
        ( set_from_ids!($($immunity),*), set_from_ids!($($weakness),*) )
    };
}

macro_rules! set_from_ids {
    ( $( $i:ident ),* ) => { HashSet::from_iter([ $( stringify!($i) ),* ].iter().cloned()) }
}

fn team_name(i: usize) -> &'static str {
    match i {
        0 => "Immune System",
        1 => "Infection",
        _ => panic!("bad team number {}", i),
    }
}

fn total_units(team: &Vec<Group>) -> usize {
    team.iter().map(|g| g.units).sum::<usize>()
}

fn battle(original_teams: &[Vec<Group>; 2], boost: usize) -> (usize, usize) {
    println!("Applying boost of: {:?}", boost);
    let mut teams = original_teams.clone();
    for group in &mut teams[0] {
        group.damage += boost;
    }

    if NOISY {
        for (t, team) in teams.iter().enumerate() {
            println!("Team {}:", team_name(t));
            for (i, group) in team.iter().enumerate() {
                println!("  {}: {}", i, group.effective_power());
            }
        }
    }

    loop {
        fn status(name: &'static str, team: &Vec<Group>) {
            println!("{} (total {} units):", name, total_units(team));
            if team.is_empty() {
                println!("No groups remain.");
            } else {
                for group in team {
                    println!("Group {} contains {} units", group.id, group.units);
                }
            }
        }

        if NOISY {
            for team in 0..=1 {
                status(team_name(team), &teams[team]);
            }
            println!();
        }

        if teams[0].is_empty() {
            return (1, total_units(&teams[1]));
        }
        if teams[1].is_empty() {
            return (0, total_units(&teams[0]));
        }

        let mut targetting_order: Vec<(usize, usize)> = (0..=1)
            .flat_map(|team| (0..teams[team].len()).map(move |i| (team, i)))
            .collect();

        targetting_order.sort_by_key(|&(team, i)| {
            let group = &teams[team][i];
            (group.effective_power(), group.initiative)
        });
        targetting_order.reverse();

        let mut unchosen: [BTreeSet<usize>; 2] = [
            BTreeSet::from_iter(0..teams[0].len()),
            BTreeSet::from_iter(0..teams[1].len()),
        ];

        let mut targets: Vec<(usize, usize, usize)> = targetting_order
            .into_iter()
            .map(|(team, i)| {
                let attacker = &teams[team][i];
                let enemy = 1 - team;
                let best_target = unchosen[enemy]
                    .iter()
                    .cloned()
                    .filter(|&group_ix| {
                        let enemy_group = &teams[enemy][group_ix];
                        attacker.damage(enemy_group) > 0
                    })
                    .max_by_key(|&group_ix| {
                        let enemy_group = &teams[enemy][group_ix];
                        //println!("{} group {} would deal defending group {} {} damage",
                        //         team_name(team), attacker.id, enemy_group.id, attacker.damage(enemy_group));
                        (
                            attacker.damage(enemy_group),
                            enemy_group.effective_power(),
                            enemy_group.initiative,
                        )
                    });
                if let Some(group_ix) = best_target {
                    unchosen[enemy].remove(&group_ix);
                }
                (team, i, best_target)
            })
            .filter_map(|(team, i, target)| target.map(|target| (team, i, target)))
            .collect();
        //println!();

        targets.sort_by_key(|&(team, i, _)| teams[team][i].initiative);
        targets.reverse();

        let mut any_killed = false;
        for &(team, i, target) in &targets {
            let killed;
            {
                let attacker = &teams[team][i];
                if attacker.units == 0 {
                    if NOISY {
                        println!(
                            "{} group {} has been defeated, and does not attack",
                            team_name(team),
                            attacker.id
                        );
                    }
                    continue;
                }

                let target = &teams[1 - team][target];
                let damage = attacker.damage(target);
                killed = min(target.units, damage / target.hp_per_unit);

                if NOISY {
                    println!(
                        "{} group {} attacks defending group {} killing {} units",
                        team_name(team),
                        attacker.id,
                        target.id,
                        killed
                    );
                }
            }
            if killed > 0 {
                any_killed = true;
            }
            teams[1 - team][target].units -= killed;
        }
        if NOISY {
            println!();
        }

        if !any_killed {
            if NOISY {
                println!("STALEMATE");
            }
            return (1, total_units(&teams[1]));
        }

        for team in teams.iter_mut() {
            team.retain(|group| group.units > 0);
        }
    }
}

const NOISY: bool = true;

fn main() {
    let mut original_teams = include!("day-24.input");
    for team in &mut original_teams {
        for (g, group) in team.iter_mut().enumerate() {
            group.id = g + 1;
        }
    }
    let original_teams = original_teams;

    let mut range = 0..5000;
    assert!(battle(&original_teams, range.start).0 == 1);
    assert!(battle(&original_teams, range.end).0 == 0);

    while range.end - range.start > 1 {
        println!("Boost range: {:?}", range);
        let boost = range.start + ((range.end - range.start) / 2) - 1;
        let (winner, units) = battle(&original_teams, boost);
        println!("{} wins with {} units left", team_name(winner), units);
        if winner == 0 {
            range.end = boost + 1;
        } else {
            range.start = boost + 1;
        }
    }
    println!("Final working boost range: {:?}", range);
}
