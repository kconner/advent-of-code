//: Playground - noun: a place where people can play

import Foundation

// Problem 1

do {
    var count = 0

    for group in data {
        if group[0] < group[1] + group[2]
            && group[1] < group[2] + group[0]
            && group[2] < group[0] + group[1]
        {
            count += 1
        }
    }
    
    let answer = count
}

// Problem 2

do {
    var count = 0

    for baseIndex in stride(from: 0, to: data.count, by: 3) {
        for i in 0..<3 {
            let group = [data[baseIndex][i], data[baseIndex + 1][i], data[baseIndex + 2][i]]
            if group[0] < group[1] + group[2]
                && group[1] < group[2] + group[0]
                && group[2] < group[0] + group[1]
            {
                count += 1
            }
        }
    }

    let answer = count
}
