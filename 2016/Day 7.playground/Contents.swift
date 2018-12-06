//: Playground - noun: a place where people can play

import Foundation

// Problem 1

do {
    func hasABBAButNotWithinBrackets(_ string: String) -> Bool {
        var foundABBA = false
        var foundABBAAfterOpenBracket = false
        var bracketNestingCount = 0
        
        var oneAgo: Character? = nil
        var twoAgo: Character? = nil
        var threeAgo: Character? = nil
        for character in string.characters {
            switch character {
            case "[":
                bracketNestingCount += 1
            case "]":
                bracketNestingCount -= 1
                
                if foundABBAAfterOpenBracket {
                    return false
                }
            default:
                if character == threeAgo && character != twoAgo && twoAgo == oneAgo {
                    foundABBA = true

                    if 0 < bracketNestingCount {
                        foundABBAAfterOpenBracket = true
                    }
                }
            }

            (threeAgo, twoAgo, oneAgo) = (twoAgo, oneAgo, character)
        }

        return foundABBA
    }

    let answer = data.reduce(0) { $0 + (hasABBAButNotWithinBrackets($1) ? 1 : 0) }
    _ = answer
}

// Problem 2

struct Match: Hashable {
    let a: Character
    let b: Character

    static func ==(lhs: Match, rhs: Match) -> Bool {
        return lhs.a == rhs.a && lhs.b == rhs.b
    }

    var hashValue: Int {
        return a.hashValue &+ b.hashValue
    }
}

do {
    func hasABAOutsideBracketsMatchingBABWithinBrackets(_ string: String) -> Bool {
        var bracketNestingCount = 0

        var matchesInsideBrackets = Set<Match>()
        var matchesOutsideBrackets = Set<Match>()
        
        var oneAgo: Character? = nil
        var twoAgo: Character? = nil
        for character in string.characters {
            switch character {
            case "[":
                bracketNestingCount += 1
            case "]":
                bracketNestingCount -= 1
            default:
                if character == twoAgo && character != oneAgo {
                    if 0 < bracketNestingCount {
                        matchesOutsideBrackets.insert(Match(a: oneAgo!, b: character))
                    } else {
                        matchesInsideBrackets.insert(Match(a: character, b: oneAgo!))
                    }
                }
            }

            (twoAgo, oneAgo) = (oneAgo, character)
        }

        return !matchesInsideBrackets.intersection(matchesOutsideBrackets).isEmpty
    }

    let answer = data.reduce(0) { $0 + (hasABAOutsideBracketsMatchingBABWithinBrackets($1) ? 1 : 0) }
    _ = answer
}
