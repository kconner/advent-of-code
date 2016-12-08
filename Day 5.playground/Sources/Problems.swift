import Foundation

public func problem2(doorID: String, initialSuffix: Int, initialCharacters: [Character?], progress: @escaping (Int, String) -> Void) -> String {
    var characters: [Character?] = initialCharacters
    var characterCount = 0

    func deliverPartialAnswer(at suffix: Int) {
        let partialAnswer = String(characters.map { $0 == nil ? "-" : $0! })
        progress(suffix, partialAnswer)
    }

    for base in stride(from: initialSuffix, to: .max, by: 1000) {
        deliverPartialAnswer(at: base)
        
        autoreleasepool {
            for suffix in base ..< (base + 1000) {
                let hash = md5Hash(of: "\(doorID)\(suffix)")
                if hash.hasPrefix("00000"),
                    let position = Int(String(hash[hash.index(hash.startIndex, offsetBy: 5)])),
                    position < 8,
                    characters[position] == nil
                {
                    characters[position] = hash[hash.index(hash.startIndex, offsetBy: 6)]
                    characterCount += 1

                    deliverPartialAnswer(at: suffix)

                    if characterCount == 8 {
                        break
                    }
                }
            }
        }
    }

    return String(characters.map { $0! })
}
