//: Playground - noun: a place where people can play

import Foundation

// Sanity checks

// should be d41d8cd98f00b204e9800998ecf8427e
md5Hash(of: "")
// should be e4d909c290d0fb1ca068ffaddf22cbd0
md5Hash(of: "The quick brown fox jumps over the lazy dog.")

// Problem 1

do {
    let doorID = "ojvtpuvg"

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
