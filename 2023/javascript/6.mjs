import * as fs from 'fs'

// Model

const makeChallenge = ([time, distance]) => ({
    totalTime: Number(time),
    distanceToBeat: Number(distance)
})

// Parsing

const input = fs.readFileSync('6.txt', 'utf8')
    .split('\n')
    .slice(0, 2)
    .map((line) =>
        /^.*:\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)$/
            .exec(line)
            .slice(1, 5)
    )

// Problem 1

// See demonstration in 6.gcx
const distance = (totalTime, holdTime) => (totalTime - holdTime) * holdTime

const rangeUp = function* (start, upperBound) {
    for (let i = start + 1; i < upperBound; ++i) { yield i }
}

const rangeDown = function* (start, lowerBound) {
    for (let i = start; i > lowerBound; --i) { yield i }
}

const assert = (condition, message) => {
    if (!condition) {
        throw new Error(message)
    }
}

const solutionCount = ({ totalTime, distanceToBeat }) => {
    const midpoint = Math.floor(totalTime / 2)

    assert(
        distanceToBeat < distance(totalTime, midpoint),
        "Falsely assumed that holding for half the time beats each record"
    )

    let count = 0

    const checkRange = (range) => {
        for (const holdTime of range) {
            if (distance(totalTime, holdTime) <= distanceToBeat) { break }
            ++count
        }
    }

    checkRange(rangeUp(midpoint, totalTime))
    checkRange(rangeDown(midpoint, 0))

    return count
}

const zip = ([seqA, seqB]) =>
    seqA.map((itemA, index) => [itemA, seqB[index]])

console.log(
    zip(input)
        .map(makeChallenge)
        .map(solutionCount)
        .reduce((a, b) => a * b, 1)
)

// Problem 2

console.log(
    solutionCount(
        makeChallenge(
            input.map((numbers) => numbers.join(''))
        )
    )
)
