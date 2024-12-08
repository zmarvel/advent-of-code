import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import gleam/option.{Some}
import gleam/regexp.{Match}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gleam/yielder
import glearray.{type Array}
import simplifile

fn parse_number(s: String) -> Int {
  case int.base_parse(s, 10) {
    Ok(n) -> n
    Error(_) -> panic
  }
}

fn parse_ordering_rule(line: String) -> #(Int, Int) {
  let assert [a, b] =
    line
    |> string.split("|")
    |> list.map(parse_number)
  #(a, b)
}

fn parse_update(line: String) -> List(Int) {
  line
  |> string.split(",")
  |> list.map(parse_number)
}

type Rule =
  #(Int, Int)

type RuleDict =
  Dict(Int, List(Rule))

fn forward_index_rules(ordering_rules: List(Rule)) -> RuleDict {
  ordering_rules
  |> list.group(fn(rule) {
    let #(a, b) = rule
    a
  })
}

pub fn index_rules(ordering_rules: List(#(Int, Int))) -> RuleDict {
  let forward_rules = forward_index_rules(ordering_rules)
  // todo I'm not sure the reverse rules are needed
  let reverse_rules =
    ordering_rules
    |> list.reverse
    |> list.group(fn(rule) {
      let #(a, b) = rule
      b
    })
  dict.combine(forward_rules, reverse_rules, fn(forward_rule, reverse_rule) {
    list.flatten([forward_rule, reverse_rule])
  })
}

fn find_rules(indexed_rules: RuleDict, x: Int) -> List(#(Int, Int)) {
  indexed_rules
  |> dict.get(x)
  |> result.unwrap([])
}

fn format_rule(r: Rule) -> String {
  let #(a, b) = r
  "rule(" <> int.to_string(a) <> "," <> int.to_string(b) <> ")"
}

fn check_step(rules: List(Rule), a: Int, b: Int) -> Bool {
  rules
  |> list.all(fn(rule) {
    case rule {
      #(x, y) if x == a && y == b -> True
      #(x, y) if x == b && y == a -> {
        // io.println(
        //   format_rule(rule)
        //   <> " a="
        //   <> int.to_string(a)
        //   <> " b="
        //   <> int.to_string(b),
        // )
        False
      }
      _ -> True
    }
  })
}

type Update =
  List(Int)

pub fn check_update(update: Update, indexed_rules: RuleDict) -> Bool {
  let update = glearray.from_list(update)
  let n = glearray.length(update)
  iterator.range(1, n)
  |> iterator.fold(True, fn(acc, i) {
    let i = n - i
    let assert Ok(curr) = glearray.get(update, i)
    let rules = find_rules(indexed_rules, curr)
    acc
    && {
      iterator.range(0, int.max(0, i - 1))
      |> iterator.all(fn(j) {
        let assert Ok(prev) = glearray.get(update, j)
        check_step(rules, prev, curr)
      })
    }
  })
}

pub fn filter_updates(rules: List(Rule), updates: List(Update)) -> List(Update) {
  let rules = index_rules(rules)
  updates
  |> list.filter(fn(update) { check_update(update, rules) })
}

pub fn sum_middle_pages(updates: List(Update)) -> Int {
  updates
  |> list.map(fn(update) {
    let update =
      update
      |> glearray.from_list
    let n = glearray.length(update)
    let assert Ok(mid) = glearray.get(update, n / 2)
    mid
  })
  |> int.sum
}

fn set_peek(s: Set(Int)) -> Int {
  let assert Ok(first) =
    s
    |> set.to_list
    |> list.first
  first
}

fn visit(
  unvisited: Set(Int),
  rules: RuleDict,
  n: Int,
  acc: Update,
) -> #(Set(Int), Update) {
  case set.contains(unvisited, n) {
    False -> #(unvisited, acc)
    _ -> {
      let edges =
        rules
        |> dict.get(n)
        |> result.unwrap([])

      io.println("n: " <> int.to_string(n))
      edges
      |> list.map(format_rule)
      |> list.each(io.println)
      let #(unvisited, acc) =
        edges
        |> list.fold(#(unvisited, acc), fn(state, edge) {
          let #(unvisited, acc) = state
          let #(_, m) = edge
          visit(unvisited, rules, m, acc)
        })
      #(set.delete(unvisited, n), [n, ..acc])
    }
  }
}

fn toposort_aux(unvisited: Set(Int), rules: RuleDict, acc: Update) {
  case set.is_empty(unvisited) {
    True -> acc
    _ -> {
      let n = set_peek(unvisited)
      let #(unvisited, acc) = visit(unvisited, rules, n, acc)
      toposort_aux(unvisited, rules, acc)
    }
  }
}

pub fn toposort(rules: List(Rule)) -> Update {
  let unvisited =
    rules
    |> list.fold(set.new(), fn(acc, rule) {
      let #(a, b) = rule
      acc
      |> set.insert(a)
      |> set.insert(b)
    })
  let rules = forward_index_rules(rules)

  toposort_aux(unvisited, rules, [])
}

fn filter_sorted_updates(
  rules: List(Rule),
  updates: List(Update),
) -> List(Update) {
  let rules = index_rules(rules)
  updates
  |> list.filter(fn(update) { !check_update(update, rules) })
}

pub fn resort_update(rules: List(Rule), update: Update) -> Update {
  let update_nums =
    update
    |> set.from_list
  let rules =
    rules
    |> list.filter(fn(rule) {
      let #(a, b) = rule
      set.contains(update_nums, a) && set.contains(update_nums, b)
    })
  toposort(rules)
}

fn read_input(path: String) -> #(List(Rule), List(Update)) {
  let assert Ok(contents) = simplifile.read(path)
  let nonempty = fn(s) { s != "" }
  let assert #(ordering_rules, updates) =
    contents
    |> string.split("\n")
    |> list.split_while(nonempty)
  let updates =
    updates
    |> list.filter(nonempty)
  #(
    ordering_rules
      |> list.map(parse_ordering_rule),
    updates
      |> list.map(parse_update),
  )
}

pub fn main() {
  let #(rules, updates) = read_input("inputs/day05.input")
  let sum =
    filter_updates(rules, updates)
    |> sum_middle_pages
  io.println("Sum (sorted): " <> int.to_string(sum))

  let sum =
    filter_sorted_updates(rules, updates)
    |> list.map(fn(update) { resort_update(rules, update) })
    |> sum_middle_pages
  io.println("Sum (resorted): " <> int.to_string(sum))
}
