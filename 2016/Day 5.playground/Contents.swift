//: Playground - noun: a place where people can play

import Foundation

// Sanity checks

// should be d41d8cd98f00b204e9800998ecf8427e
md5Hash(of: "")
// should be e4d909c290d0fb1ca068ffaddf22cbd0
md5Hash(of: "The quick brown fox jumps over the lazy dog.")

let doorID = "ojvtpuvg"

// Problem 1

do {
    var password: String = ""

    for suffix in 0 ..< .max {
        let hash = md5Hash(of: "\(doorID)\(suffix)")
        if hash.hasPrefix("00000") {
            password.append(hash[hash.index(hash.startIndex, offsetBy: 5)])
            if password.characters.count == 8 {
                break
            }
        }
    }

    let answer = password
}

// Problem 2

do {
    let answer = problem2(doorID: doorID, initialSuffix: 27_419_000, initialCharacters: [nil, "0", "5", "0", "c", "b", "b", "d"], progress: { (hashesTried, partialAnswer) in
        _ = hashesTried
        _ = partialAnswer
    })
    _ = answer
}
