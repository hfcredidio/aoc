use std::f64::consts::PI;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;
use std::str::Chars;

#[derive(Debug)]
enum Action {
    North(i32),
    South(i32),
    East(i32),
    West(i32),
    Left(i32),
    Right(i32),
    Forward(i32),
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    fn rotate(self, deg: i32) -> Direction {
        match deg {
            90 => match self {
                Direction::North => Direction::West,
                Direction::West => Direction::South,
                Direction::South => Direction::East,
                Direction::East => Direction::North,
            },
            -90 => match self {
                Direction::North => Direction::East,
                Direction::East => Direction::South,
                Direction::South => Direction::West,
                Direction::West => Direction::North,
            },
            180 => self.rotate(90).rotate(90),
            -180 => self.rotate(-90).rotate(-90),
            270 => self.rotate(180).rotate(90),
            -270 => self.rotate(-180).rotate(-90),
            _ => panic!("invalid rotation value"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Position {
    east: i32,
    north: i32,
}

impl Position {
    pub fn rotate(&self, deg: i32) -> Position {
        let c = (deg as f64 * PI / 180.0).cos();
        let s = (deg as f64 * PI / 180.0).sin();
        let x = self.east as f64;
        let y = self.north as f64;
        Position {
            east: (c * x - s * y).round() as i32,
            north: (s * x + c * y).round() as i32,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Ship {
    position: Position,
    direction: Direction,
}

impl Ship {
    pub fn new(east: i32, north: i32, dir: Direction) -> Self {
        Self {
            position: Position {
                east: east,
                north: north,
            },
            direction: dir,
        }
    }

    pub fn apply(&self, action: Action) -> Self {
        match action {
            Action::North(v) => {
                Ship::new(self.position.east, self.position.north + v, self.direction)
            }
            Action::South(v) => {
                Ship::new(self.position.east, self.position.north - v, self.direction)
            }
            Action::East(v) => {
                Ship::new(self.position.east + v, self.position.north, self.direction)
            }
            Action::West(v) => {
                Ship::new(self.position.east - v, self.position.north, self.direction)
            }
            Action::Left(v) => Ship::new(
                self.position.east,
                self.position.north,
                self.direction.rotate(v),
            ),
            Action::Right(v) => Ship::new(
                self.position.east,
                self.position.north,
                self.direction.rotate(-v),
            ),
            Action::Forward(v) => self.apply(match self.direction {
                Direction::North => Action::North(v),
                Direction::East => Action::East(v),
                Direction::South => Action::South(v),
                Direction::West => Action::West(v),
            }),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct WaypointShip {
    position: Position,
    waypoint: Position,
}

impl WaypointShip {
    pub fn new(east: i32, north: i32, weast: i32, wnorth: i32) -> Self {
        Self {
            position: Position {
                east: east,
                north: north,
            },
            waypoint: Position {
                east: weast,
                north: wnorth,
            },
        }
    }

    pub fn apply(&self, action: Action) -> Self {
        match action {
            Action::North(v) => WaypointShip::new(
                self.position.east,
                self.position.north,
                self.waypoint.east,
                self.waypoint.north + v,
            ),
            Action::South(v) => WaypointShip::new(
                self.position.east,
                self.position.north,
                self.waypoint.east,
                self.waypoint.north - v,
            ),
            Action::East(v) => WaypointShip::new(
                self.position.east,
                self.position.north,
                self.waypoint.east + v,
                self.waypoint.north,
            ),
            Action::West(v) => WaypointShip::new(
                self.position.east,
                self.position.north,
                self.waypoint.east - v,
                self.waypoint.north,
            ),
            Action::Left(v) => {
                let wp = self.waypoint.rotate(v);
                WaypointShip::new(self.position.east, self.position.north, wp.east, wp.north)
            }
            Action::Right(v) => {
                let wp = self.waypoint.rotate(-v);
                WaypointShip::new(self.position.east, self.position.north, wp.east, wp.north)
            }
            Action::Forward(v) => WaypointShip::new(
                self.position.east + v * self.waypoint.east,
                self.position.north + v * self.waypoint.north,
                self.waypoint.east,
                self.waypoint.north,
            ),
        }
    }
}

fn parse_action(mut s: Chars) -> Action {
    let parse = |it: Chars| it.collect::<String>().parse::<i32>().unwrap();
    match s.next() {
        Some('N') => Action::North(parse(s)),
        Some('S') => Action::South(parse(s)),
        Some('E') => Action::East(parse(s)),
        Some('W') => Action::West(parse(s)),
        Some('L') => Action::Left(parse(s)),
        Some('R') => Action::Right(parse(s)),
        Some('F') => Action::Forward(parse(s)),
        _ => panic!("Invalid line"),
    }
}

pub fn day12(path: &Path) {
    let file = File::open(path).unwrap();
    let ship = BufReader::new(file)
        .lines()
        .map(|x| x.unwrap())
        .map(|line| parse_action(line.chars()))
        .fold(Ship::new(0, 0, Direction::East), |ship, action| {
            ship.apply(action)
        });
    println!("{:?}", ship.position.east.abs() + ship.position.north.abs());

    let file = File::open(path).unwrap();
    let wship = BufReader::new(file)
        .lines()
        .map(|x| x.unwrap())
        .map(|line| parse_action(line.chars()))
        .fold(WaypointShip::new(0, 0, 10, 1), |ship, action| {
            ship.apply(action)
        });
    println!(
        "{:?}",
        wship.position.east.abs() + wship.position.north.abs()
    );
}
