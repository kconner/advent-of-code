//: Playground - noun: a place where people can play

import Foundation

// Problem 1
/*
do {
    var sum = 0

    for room in data {
        let parts = room.characters.split { character in
            return character == "[" || character == "]"
        }
        let checksum = parts[1]
        let reversedSubparts = parts[0]
            .lazy
            .reversed()
            .split(separator: "-", maxSplits: 1, omittingEmptySubsequences: true)
        let name = reversedSubparts[1]

        var characterFrequencies: [Character : Int] = [:]
        for character in name.filter({ $0 != "-" }) {
            if let frequency = characterFrequencies[character] {
                characterFrequencies[character] = frequency + 1
            } else {
                characterFrequencies[character] = 1
            }
        }

        let highestFrequencyCharacters = characterFrequencies
            .sorted { pair1, pair2 in
                let (character1, count1) = pair1
                let (character2, count2) = pair2
                return count2 < count1 || (count1 == count2 && character1 < character2)
            }
            .map { (character, _) in character }

        let valid = highestFrequencyCharacters.starts(with: checksum)
        if valid {
            let sectorID = Int(String(reversedSubparts[0].reversed()))!
            sum += sectorID
        }
    }

    let answer = sum
}
 */

// Problem 2

do {
    let letters = Array("abcdefghijklmnopqrstuvwxyz".characters)
    var letterIndexes: [Character : Int] = [:]
    for (index, letter) in letters.enumerated() {
        letterIndexes[letter] = index
    }

    let wantedRoomName = "northpole object storage".characters
    var matchingSectorID: Int = 0

    for room in data {
        let parts = room.characters.split { character in
            return character == "[" || character == "]"
        }
        let checksum = parts[1]
        let reversedSubparts = parts[0]
            .lazy
            .reversed()
            .split(separator: "-", maxSplits: 1, omittingEmptySubsequences: true)
        let name = reversedSubparts[1].reversed()
        let sectorID = Int(String(reversedSubparts[0].reversed()))!

        let decryptedName: [Character] = name.map { character in
            guard character != "-" else {
                return " "
            }

            return letters[(letterIndexes[character]! + sectorID) % letters.count]
        }

        if decryptedName.starts(with: wantedRoomName) {
            matchingSectorID = sectorID
        }
    }

    let answer = matchingSectorID
}
