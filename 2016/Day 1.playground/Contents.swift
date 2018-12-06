//: Playground - noun: a place where people can play

import Foundation

struct Instruction {

    enum Rotation {
        case left, right
    }

    let rotation: Rotation?
    let distance: Int

    init(turn rotation: Rotation?, move distance: Int) {
        self.rotation = rotation
        self.distance = distance
    }

}

struct Car {

    enum Direction {

        case north, east, south, west

        mutating func turn(_ rotation: Instruction.Rotation) {
            switch rotation {
            case .left:
                self = toLeft
            case .right:
                self = toRight
            }
        }

        // Helpers

        private var toRight: Direction {
            switch self {
            case .north:
                return .east
            case .east:
                return .south
            case .south:
                return .west
            case .west:
                return .north
            }
        }

        private var toLeft: Direction {
            return toRight.toRight.toRight // lolz
        }

    }

    struct Location {
        
        var x: Int
        var y: Int

        var manhattanDistance: Int {
            return abs(x) + abs(y)
        }

        mutating func move(_ direction: Direction, by distance: Int) {
            switch direction {
            case .north:
                y += distance
            case .east:
                x += distance
            case .south:
                y -= distance
            case .west:
                x -= distance
            }
        }

    }

    var direction: Direction
    var location: Location

    init() {
        direction = .north
        location = Location(x: 0, y: 0)
    }

    mutating func perform(_ instruction: Instruction) {
        if let rotation = instruction.rotation {
            direction.turn(rotation)
        }

        location.move(direction, by: instruction.distance)
    }

}

// Test data

var data: [(Instruction.Rotation, Int)] = []
data.append((.left, 5))
data.append((.right, 1))
data.append((.left, 5))
data.append((.left, 1))
data.append((.right, 5))
data.append((.right, 1))
data.append((.right, 1))
data.append((.left, 4))
data.append((.left, 1))
data.append((.left, 3))
data.append((.right, 2))
data.append((.right, 4))
data.append((.left, 4))
data.append((.left, 1))
data.append((.left, 1))
data.append((.right, 2))
data.append((.right, 4))
data.append((.right, 3))
data.append((.left, 1))
data.append((.right, 4))
data.append((.left, 4))
data.append((.left, 5))
data.append((.left, 4))
data.append((.right, 4))
data.append((.left, 5))
data.append((.right, 1))
data.append((.right, 5))
data.append((.left, 2))
data.append((.right, 1))
data.append((.right, 3))
data.append((.left, 2))
data.append((.left, 4))
data.append((.left, 4))
data.append((.right, 1))
data.append((.left, 192))
data.append((.right, 5))
data.append((.right, 1))
data.append((.right, 4))
data.append((.left, 5))
data.append((.left, 4))
data.append((.right, 5))
data.append((.left, 1))
data.append((.left, 1))
data.append((.right, 48))
data.append((.right, 5))
data.append((.right, 5))
data.append((.left, 2))
data.append((.right, 4))
data.append((.right, 4))
data.append((.right, 1))
data.append((.right, 3))
data.append((.left, 1))
data.append((.left, 4))
data.append((.left, 5))
data.append((.right, 1))
data.append((.left, 4))
data.append((.left, 2))
data.append((.left, 5))
data.append((.right, 5))
data.append((.left, 2))
data.append((.right, 74))
data.append((.right, 4))
data.append((.left, 1))
data.append((.right, 188))
data.append((.right, 5))
data.append((.left, 4))
data.append((.left, 2))
data.append((.right, 5))
data.append((.right, 2))
data.append((.left, 4))
data.append((.right, 4))
data.append((.right, 3))
data.append((.right, 3))
data.append((.right, 2))
data.append((.right, 1))
data.append((.left, 3))
data.append((.left, 2))
data.append((.left, 5))
data.append((.left, 5))
data.append((.left, 2))
data.append((.left, 1))
data.append((.right, 1))
data.append((.right, 5))
data.append((.right, 4))
data.append((.left, 3))
data.append((.right, 5))
data.append((.left, 1))
data.append((.left, 3))
data.append((.right, 4))
data.append((.left, 1))
data.append((.left, 3))
data.append((.left, 2))
data.append((.right, 1))
data.append((.right, 3))
data.append((.right, 2))
data.append((.right, 5))
data.append((.left, 3))
data.append((.left, 1))
data.append((.left, 1))
data.append((.right, 5))
data.append((.left, 4))
data.append((.left, 5))
data.append((.right, 5))
data.append((.right, 2))
data.append((.left, 5))
data.append((.right, 2))
data.append((.left, 1))
data.append((.left, 5))
data.append((.left, 3))
data.append((.left, 5))
data.append((.left, 5))
data.append((.left, 1))
data.append((.right, 1))
data.append((.left, 4))
data.append((.left, 3))
data.append((.left, 1))
data.append((.right, 2))
data.append((.right, 5))
data.append((.left, 1))
data.append((.left, 3))
data.append((.right, 4))
data.append((.right, 5))
data.append((.left, 4))
data.append((.left, 1))
data.append((.right, 5))
data.append((.left, 1))
data.append((.right, 5))
data.append((.right, 5))
data.append((.right, 5))
data.append((.right, 2))
data.append((.right, 1))
data.append((.right, 2))
data.append((.left, 5))
data.append((.left, 5))
data.append((.left, 5))
data.append((.right, 4))
data.append((.left, 5))
data.append((.left, 4))
data.append((.left, 4))
data.append((.right, 5))
data.append((.left, 2))
data.append((.right, 1))
data.append((.right, 5))
data.append((.left, 1))
data.append((.left, 5))
data.append((.right, 4))
data.append((.left, 3))
data.append((.right, 4))
data.append((.left, 2))
data.append((.right, 3))
data.append((.right, 3))
data.append((.right, 3))
data.append((.left, 2))
data.append((.left, 2))
data.append((.left, 2))
data.append((.left, 1))
data.append((.left, 4))
data.append((.right, 3))
data.append((.left, 4))
data.append((.left, 2))
data.append((.right, 2))
data.append((.right, 5))
data.append((.left, 1))
data.append((.right, 2))
let instructions = data.map(Instruction.init)

// Problem 1

do {
    var car = Car()

    for instruction in instructions {
        car.perform(instruction)
    }

    let answer = car.location.manhattanDistance
}

// Problem 2

extension Instruction {

    var steps: FlattenBidirectionalCollection<[Repeated<Instruction>]> {
        let headStep = Instruction(turn: rotation, move: 1)
        let tailStep = Instruction(turn: nil, move: 1)

        return [
            repeatElement(headStep, count: 1),
            repeatElement(tailStep, count: distance - 1)
        ].joined()
    }

}

extension Car.Location : Hashable {

    // Equatable
    
    static func ==(lhs: Car.Location, rhs: Car.Location) -> Bool {
        return lhs.x == rhs.x
            && lhs.y == rhs.y
    }
    
    // Hashable
    
    var hashValue: Int {
        return x * 1000 + y
    }

}

do {
    var car = Car()
    let steps = instructions.lazy.flatMap {
        $0.steps
    }

    var visitedLocations: Set<Car.Location> = [car.location]
    
    for step in steps {
        car.perform(step)

        let location = car.location
        if visitedLocations.contains(location) {
            break
        }
        visitedLocations.insert(location)
    }

    let answer = car.location.manhattanDistance
}
