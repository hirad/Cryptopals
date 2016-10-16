//: [Previous](@previous)

import Foundation

// The hex encoded string:
let str = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
// ... has been XOR'd against a single character. Find the key, decrypt the message.
// You can do this by hand. But don't: write code to do it for you.
// How? Devise some method for "scoring" a piece of English plaintext.
// Character frequency is a good metric.
// Evaluate each output and choose the one with the best score.

// most common english letters
let common = ["E", "T", "A", "O", "I", "N", "S", "H", "R", "D", "L", "U"].map { UInt8($0.lowercased().unicodeScalars.first!.value) }

let decoded = hexDecode(str)

func decipherXOR(message: [UInt8], withKey key: UInt8) -> String {
    return message.map { code -> String in
        let decoded = code ^ key
        return String(UnicodeScalar(decoded))
    }.joined()
}

let candidates = (0...UInt8.max).map { decipherXOR(message: decoded, withKey: $0) }

func score(text: String) -> Int {
    // naively, the more characters in the commons list above, the better the score
    let chars = text.unicodeScalars
    return chars.reduce(0) { acc, char -> Int in
        let asByte = UInt8(char.value)
        if common.contains(asByte) {
            return acc + (common.count - (common.index(of: asByte) ?? common.count)) + 3
        }
        else if asByte >= 65 && asByte <= 90 && common.contains(asByte + 32) {
            // it's a capital letter - slightly lower score
            let lowercaseByte = asByte + 32
            return acc + (common.count - (common.index(of: lowercaseByte) ?? common.count)) + 1
        }
        else {
            // reduce the weight of symbols
            switch asByte {
            case 65...90: // it's a uppercase letter
                return acc + 1
            case 97...122: // it's a lowercase letter
                return acc + 2
            case 48...57, 32: // it's a number
                return acc
            default: // lower the weight of anything that's none of the above
                return acc - 3
            }
        }
    }
}

let scoredCandidates = candidates.map { ($0, score(text: $0)) }.sorted { (candidate1: (String, Int), candidate2: (String, Int)) -> Bool in
    return candidate1.1 > candidate2.1
}

print("Top candidate: \(scoredCandidates.first?.0)")


//: [Next](@next)
