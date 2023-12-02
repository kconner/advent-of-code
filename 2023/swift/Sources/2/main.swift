import Parsing

// Model

struct Game {
    let id: Int
    let handfuls: [Handful]
}

struct Handful {
    let countsByColor: [Color: Int]

    init(countsAndColors: [(Int, Color?)]) {
        self.countsByColor = Dictionary(
            uniqueKeysWithValues: countsAndColors.map { ($1!, $0) }
        )
    }
}

enum Color: String {
    case red = "red"
    case green = "green"
    case blue = "blue"
}

// Parsing

let terminals: Set<Character> = [",", ";", "\n"]
let colorParser = Parse(input: Substring.self, Color.init(rawValue:)) {
    Prefix { !terminals.contains($0) }.map(String.init)
}

let handfulParser = Parse(input: Substring.self, Handful.init(countsAndColors:)) {
    Many {
        Int.parser()
        " "
        colorParser
    } separator: {
        ", "
    }
} 

let gameParser = Parse(input: Substring.self, Game.init(id:handfuls:)) {
    "Game "
    Int.parser()
    ": "
    Many {
        handfulParser
    } separator: {
        "; "
    }
}

let inputParser = Parse(input: Substring.self) {
    Many {
        gameParser
    } separator: {
        "\n"
    }
}

let games = try! inputParser.parse(String(contentsOfFile: "2.txt").dropLast())

// Problem 1

extension Game {
    func lowerBound(of color: Color) -> Int {
        handfuls.compactMap(\.countsByColor[color]).reduce(0, max)
    }

    func mightHave(atMost upperBound: Int, of color: Color) -> Bool {
        lowerBound(of: color) <= upperBound
    }
}

print(
    games
        .filter { $0.mightHave(atMost: 12, of: .red) }
        .filter { $0.mightHave(atMost: 13, of: .green) }
        .filter { $0.mightHave(atMost: 14, of: .blue) }
        .map(\.id)
        .reduce(0, +)
)

// Problem 2

extension Color: CaseIterable {}

extension Game {
    var power: Int {
        Color.allCases.map(lowerBound(of:)).reduce(1, *)
    }
}

print(games.map(\.power).reduce(0, +))
