//: Playground - noun: a place where people can play

import Foundation

func bestCharactersByColumn(data: [String], initial: Int, comparator: (Int, Int) -> Bool) -> String {
    // Transpose
    var dataIterators = data.map { $0.characters.makeIterator() }
    let dataColumns: [[Character]] = (0 ..< 8).map { _ in
        (0 ..< data.count).map { index in
            dataIterators[index].next()!
        }
    }

    let characters: [Character] = dataColumns.flatMap { column in
        var iterator = column.sorted().makeIterator()
        var current = (character: iterator.next(), count: 1)
        var best = (character: current.character, count: initial)

        for character in AnySequence(iterator) {
            if character == current.character {
                current.count += 1
            } else {
                if comparator(current.count, best.count) {
                    best = current
                }

                current = (character: character, count: 1)
            }
        }

        return best.character
    }

    return String(characters)
}

let answer1 = bestCharactersByColumn(data: data, initial: 1, comparator: >=)
let answer2 = bestCharactersByColumn(data: data, initial: .max, comparator: <)
