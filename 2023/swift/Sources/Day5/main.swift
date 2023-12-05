import Foundation
import Parsing

// Model

struct Input {
    let seedItems: [Item]
    let maps: [Map]
}

struct Item {
    let index: Int
    let kind: String
}

struct Map {
    let sourceKind: String
    let destinationKind: String
    let rangeMappings: [RangeMapping]
}

struct RangeMapping {
    let sourceRange: Range<Int>
    let destination: Int

    init(destination: Int, source: Int, length: Int) {
        self.sourceRange = source ..< source + length
        self.destination = destination
    }
}

// Parsing

let rangeMappingParser = Parse(input: Substring.self, RangeMapping.init) {
    Int.parser()
    " "
    Int.parser()
    " "
    Int.parser()
}

let mapParser = Parse(input: Substring.self, Map.init(sourceKind:destinationKind:rangeMappings:)) {
    Prefix { $0 != "-" }.map(String.init)
    "-to-"
    Prefix { $0 != " " }.map(String.init)
    " map:"

    Whitespace(1, .vertical)

    Many { rangeMappingParser } separator: { Whitespace(1, .vertical) }
}

let inputParser = Parse(input: Substring.self, Input.init) {
    "seeds: "
    Many {
        Int.parser().map { Item(index: $0, kind: "seed") }
    } separator: {
        " "
    }
    Whitespace(2, .vertical)

    Many { mapParser } separator: { Whitespace(2, .vertical) }
    Whitespace(1, .vertical)
}

var input = try! inputParser.parse(String(contentsOfFile: "5.txt"))

// Problem 1

extension RangeMapping {
    subscript(index: Int) -> Int {
        assert(sourceRange.contains(index))

        return destination + index - sourceRange.lowerBound
    } 
}

extension Map {
    subscript(item: Item) -> Item {
        assert(item.kind == sourceKind)

        for mapping in rangeMappings where mapping.sourceRange.contains(item.index) {
            return Item(index: mapping[item.index], kind: destinationKind)
        }
        
        return Item(index: item.index, kind: destinationKind)
    }
}

print(
    input.seedItems
        .map { item in
            input.maps.reduce(item) { item, map in
                map[item]
            }
        }
        .reduce(Int.max) { value, item in
            min(value, item.index)
        }
)

// Problem 2

struct ItemRanges {
    let indices: IndexSet
    let kind: String
}

extension Input {
    var seedIndexSet: ItemRanges {
        let ranges = zip(
            stride(from: 0, to: seedItems.count, by: 2).map { seedItems[$0].index },
            stride(from: 1, to: seedItems.count, by: 2).map { seedItems[$0].index }
        ).map { (offset, length) in
            offset ..< offset + length
        }

        var indices = IndexSet()
        for range in ranges {
            indices.insert(integersIn: range.indices)
        }

        return ItemRanges(indices: indices, kind: "seed")
    }
}

extension RangeMapping {
    subscript(range: Range<Int>) -> Range<Int> {
        assert(range.clamped(to: sourceRange) == range)

        return destination + range.lowerBound - sourceRange.lowerBound
            ..< destination + range.upperBound - sourceRange.lowerBound
    } 
}

extension Map {
    subscript(itemRanges: ItemRanges) -> ItemRanges {
        assert(itemRanges.kind == sourceKind)

        var remainingIndices = itemRanges.indices
        var resultIndices = IndexSet()

        for range in remainingIndices.rangeView {
            for mapping in rangeMappings {
                let intersection = range.clamped(to: mapping.sourceRange)
                guard !intersection.isEmpty else { continue }

                remainingIndices.remove(integersIn: intersection)
                resultIndices.insert(integersIn: mapping[intersection])
            }
        }

        return ItemRanges(
            indices: resultIndices.union(remainingIndices),
            kind: destinationKind
        )
    }
}

print(
    input.maps
        .reduce(input.seedIndexSet) { itemRanges, map in
            map[itemRanges]
        }
        .indices
        .first!
)
