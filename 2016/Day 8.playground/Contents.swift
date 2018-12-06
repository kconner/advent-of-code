import Foundation

struct Screen: CustomStringConvertible {

    var bitmap = Array(repeating: Array(repeating: false, count: 50), count: 6)

    mutating func perform(_ instruction: Instruction) {
        _ = ()
        switch instruction {
        case .fill(let width, let height):
            for row in 0 ..< height {
                for column in 0 ..< width {
                    bitmap[row][column] = true
                }
            }
        case .rotateRow(let index, let right):
            let row = bitmap[index]
            let cutIndex = row.count - right
            bitmap[index] = Array([row[cutIndex ..< row.count], row[0 ..< cutIndex]].joined())
        case .rotateColumn(let index, let down):
            let column = bitmap.map { $0[index] }
            let cutIndex = column.count - down
            let rotatedColumn = Array([column[cutIndex ..< column.count], column[0 ..< cutIndex]].joined())
            for row in 0 ..< rotatedColumn.count {
                bitmap[row][index] = rotatedColumn[row]
            }
        }
    }

    var litPixelCount: Int {
        return bitmap.reduce(0) { $0 + $1.reduce(0) { $0 + ($1 ? 1 : 0) } }
    }

    var description: String {
        return bitmap.map { row in
            row.map { pixel in
                pixel ? "x" : "."
            }.joined(separator: "")
        }.joined(separator: "\n")
    }

}

var screen = Screen()

for instruction in data {
    screen.perform(instruction)
}

let answer1 = screen.litPixelCount
let answer2 = screen.description
print(answer2)
