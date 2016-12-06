import Foundation

struct Byte: Sequence {

    var value: UInt8

    init(_ value: UInt8) {
        self.value = value
    }

    init(bits: [Bool]) {
        self.value = 0

        for (index, bit) in bits.enumerated() {
            self[UInt8(index)] = bit
        }
    }

    static func bytes(from value: UInt64) -> [Byte] {
        var bytes: [Byte] = []
        var shiftedValue = value
        for _ in 0 ..< 8 {
            let byte = Byte(UInt8(shiftedValue % 0x100))
            bytes.append(byte)
            shiftedValue >>= 8
        }
        return bytes
    }

    static func bytes(from value: UInt32) -> [Byte] {
        var bytes: [Byte] = []
        var shiftedValue = value
        for _ in 0 ..< 4 {
            let byte = Byte(UInt8(shiftedValue % 0x100))
            bytes.append(byte)
            shiftedValue >>= 8
        }
        return bytes
    }

    static func uint32(from bytes: [Byte]) -> UInt32 {
        precondition(bytes.count == 4)

        var value: UInt32 = 0
        for (index, byte) in bytes.enumerated() {
            let byteValue = UInt32(byte.value) << (8 * UInt32(index))
            value += byteValue
        }
        return value
    }

    // This is in memory order, so index 0 is the most significant bit.
    subscript(index: UInt8) -> Bool {
        get {
            let place = 7 - index
            let bit = 1 << place

            return value & bit != 0
        }
        set {
            let place = 7 - index
            let bit = 1 << place
            
            value &= ~bit // clear the bit
            
            if newValue {
                value |= bit // set it
            }
        }
    }

    // Sequence

    func makeIterator() -> IndexingIterator<[Bool]> {
        return (0 ..< 8).map { self[$0] }.makeIterator()
    }

}

struct MD5HashByteArray {

    private var bytes: [Byte]

    init(string: String) {
        bytes = []

        for byte: CChar in string.utf8CString.dropLast() {
            append(Byte(UInt8(byte)))
        }
    }

    var count: Int {
        return bytes.count
    }

    mutating func append(_ byte: Byte) {
        bytes.append(byte)
    }

    mutating func append(_ value: UInt64) {
        bytes.append(contentsOf: Byte.bytes(from: value))
    }

    var wordChunks: [[UInt32]] {
        return stride(from: 0, to: bytes.count, by: 64).map { chunkOffset in
            stride(from: chunkOffset, to: chunkOffset + 64, by: 4).map { wordOffset in
                Byte.uint32(from: Array(bytes[wordOffset ..< wordOffset + 4]))
            }
        }
    }

}

func rotateLeft(value x: UInt32, by c: UInt32) -> UInt32 {
    return (x << c) | (x >> (32 - c))
}

func hexadecimalString(from number: UInt8) -> String {
    return String(format: "%02x", number)
}

public func md5Hash(of string: String) -> String {
    let s: [UInt32] = [
        7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
        5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
        4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
        6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
    ]

    let K: [UInt32] = [
        0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
        0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
        0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
        0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
        0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
        0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
        0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
        0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
        0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
        0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
        0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
        0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
        0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
        0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
        0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
        0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
    ]

    var a0: UInt32 = 0x67452301
    var b0: UInt32 = 0xefcdab89
    var c0: UInt32 = 0x98badcfe
    var d0: UInt32 = 0x10325476

    var bytes = MD5HashByteArray(string: string)
    let bitCount = bytes.count * 8

    bytes.append(Byte(bits: [true, false, false, false, false, false, false, false]))
    while bytes.count % 64 != 56 {
        bytes.append(Byte(0))
    }

    bytes.append(UInt64(bitCount))

    for M in bytes.wordChunks {
        print(M)
        var A = a0
        var B = b0
        var C = c0
        var D = d0

        for i in 0 ..< 64 {
            let F: UInt32
            let g: Int

            switch i {
            case 0 ..< 16:
                F = (B & C) | ((~B) & D)
                g = i
            case 16 ..< 32:
                F = (D & B) | ((~D) & C)
                g = (5 * i + 1) % 16
            case 32 ..< 48:
                F = B ^ C ^ D
                g = (3 * i + 5) % 16
            default:
                F = C ^ (B | (~D))
                g = (7 * i) % 16
            }

            let dTemp = D
            D = C
            C = B
            B = B &+ rotateLeft(value: (A &+ F &+ K[i] &+ M[g]), by: s[i])
            A = dTemp
        }

        a0 = a0 &+ A
        b0 = b0 &+ B
        c0 = c0 &+ C
        d0 = d0 &+ D
    }

    var result = ""
    for word in [a0, b0, c0, d0] {
        for byte in Byte.bytes(from: word) {
            result.append(hexadecimalString(from: byte.value))
        }
    }
    return result
}
