fn find_interval(schedule: &Vec<(i64, i64)>) -> i64 {
    let mut it = schedule.iter().peekable();
    let (mut t, mut step) = it.next().unwrap();
    loop {
        match it.peek() {
            None => return t,
            Some((k, n)) if (t + k) % n == 0 => {
                it.next();
                step *= n;
            }
            _ => {
                t += step;
            }
        }
    }
}

pub fn day13() {
    let schedule = "19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,751,x,29,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,431,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17"
        .split(',')
        .enumerate()
        .filter(|(_, x)| x != &"x")
        .map(|(i, v)| (i as i64, v.parse::<i64>().unwrap()))
        .collect::<Vec<(i64, i64)>>();

    let depart_time = 1002578;
    let (bus, time) = schedule
        .iter()
        .map(|(_, v)| (v, v - (depart_time % v)))
        .min_by_key(|x| x.1)
        .unwrap();

    println!("{:?}", bus * time);

    println!("{:?}", find_interval(&schedule));
}
