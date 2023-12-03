import Foundation

// Model

struct Rect {
    let rows: ClosedRange<Int>
    let columns: ClosedRange<Int>

    func overlaps(_ other: Rect) -> Bool {
        rows.overlaps(other.rows) && columns.overlaps(other.columns)
    }
}

struct Number {
    let value: Int
    let rect: Rect

    init(value: Substring, row: Int, column: Int) {
        self.value = Int(value)!

        rect = Rect(
            rows: row...row,
            columns: column...(column + value.count - 1)
        )
    }
}

struct Symbol {
    let adjacentRect: Rect
    let isGear: Bool

    init(value: Substring, row: Int, column: Int) {
        adjacentRect = Rect(
            rows: (row - 1)...(row + 1),
            columns: (column - 1)...(column + 1)
        )

        isGear = value == "*"
    }
}

// Parsing

// Something like this has to exist in the SDK and I just didn't find it, right?
struct MatchSequence: Sequence, IteratorProtocol {

    typealias Element = (output: Substring, column: Int)

    let regex: Regex<Substring>
    let line: Substring
    var offset: Substring.Index

    init(matching regex: Regex<Substring>, in line: Substring) {
        self.regex = regex
        self.line = line
        self.offset = line.startIndex
    }

    mutating func next() -> Element? {
        guard let match = try! regex.firstMatch(in: line.suffix(from: offset)) else {
            return nil
        }

        defer {
            offset = match.output.endIndex
        }

        return (
            output: match.output,
            column: line.distance(from: line.startIndex, to: match.output.startIndex)
        )
    }
}

let lines = try! String(contentsOfFile: "3.txt").split(separator: "\n")

let numbers = lines.enumerated().flatMap { (row, line) in
    MatchSequence(matching: #/\d+/#, in: line).map { (value, column) in
        Number(value: value, row: row, column: column)
    }
}

let symbols = lines.enumerated().flatMap { (row, line) in
    MatchSequence(matching: #/[^\d\.]/#, in: line).map { (value, column) in
        Symbol(value: value, row: row, column: column)
    }
}

// Problem 1

extension Symbol {
    func isAdjacentTo(_ number: Number) -> Bool {
        adjacentRect.overlaps(number.rect)
    }
}

extension Number {
    func isAdjacentTo(anyOf symbols: [Symbol]) -> Bool {
        symbols.contains { symbol in
            symbol.isAdjacentTo(self)
        }
    }
}

print(
    numbers
        .filter { $0.isAdjacentTo(anyOf: symbols) }
        .map(\.value)
        .reduce(0, +)
)

// Problem 2

extension Symbol {
    func adjacentNumberValues(among numbers: [Number]) -> [Int] {
        numbers.filter { self.isAdjacentTo($0) }.map(\.value)
    }
}

print(
    symbols
        .filter(\.isGear)
        .map { $0.adjacentNumberValues(among: numbers) }
        .filter { $0.count == 2 }
        .map { $0.reduce(1, *) }
        .reduce(0, +)
        as Int
)
