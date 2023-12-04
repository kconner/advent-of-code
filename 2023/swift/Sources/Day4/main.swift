import Parsing

// Model

struct Card {
    let id: Int
    let winningNumbers: Set<Int>
    let yourNumbers: [Int]
    var inality = 1

    init(id: Int, winningNumbers: Set<Int>, yourNumbers: [Int]) {
        self.id = id
        self.winningNumbers = winningNumbers
        self.yourNumbers = yourNumbers
    }
}

// Parsing

let cardParser = Parse(input: Substring.self, Card.init(id:winningNumbers:yourNumbers:)) {
    "Card"

    Whitespace()

    Int.parser()
    ":"

    Whitespace()

    Many { Int.parser() } separator: { Whitespace() }.map(Set.init)

    Whitespace()
    "|"
    Whitespace()

    Many { Int.parser() } separator: { Whitespace() }
}

let inputParser = Parse(input: Substring.self) {
    Many { cardParser } separator: { Whitespace(1, .vertical) }
    Whitespace(1, .vertical)
}

var cards = try! inputParser.parse(String(contentsOfFile: "4.txt"))

// Problem 1

extension Card {
    var matchCount: Int {
        // Whatever happened to .count(where:) ?
        yourNumbers.lazy.filter(winningNumbers.contains).count
    }

    var pointCount: Int {
        1 << (matchCount - 1)
    }
}

print(cards.map(\.pointCount).reduce(0, +))

// Problem 2

var cardIndices = cards.indices
for index in cardIndices {
    let card = cards[index]
    let copyIndices = (index + 1 ..< index + 1 + card.matchCount).clamped(to: cardIndices)
    for otherIndex in copyIndices {
        cards[otherIndex].inality += card.inality
    }
}

print(cards.map(\.inality).reduce(0, +))
