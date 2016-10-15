//: Playground - noun: a place where people can play

import UIKit

// Set 1, challenge 1
// Convert hex to base64
// This input...
let hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
// should produce...
let expectedBase64 = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

extension String {
    var asciiArray: [UInt32] {
        return unicodeScalars.filter{ $0.isASCII }.map{ $0.value }
    }
}

extension Character {
    var asciiValue: UInt32? {
        return String(self).unicodeScalars.filter{ $0.isASCII }.first?.value
    }
}

func hexToByte(hexStr: String) -> UInt8 {
    let length = hexStr.characters.count
    guard length == 2 else {
        fatalError("hexToByte(_:) should be called with a 2-digit hex string")
    }
    
    let hexValues: [String: UInt8] = [
        "0": 0,
        "1": 1,
        "2": 2,
        "3": 3,
        "4": 4,
        "5": 5,
        "6": 6,
        "7": 7,
        "8": 8,
        "9": 9,
        "A": 10,
        "B": 11,
        "C": 12,
        "D": 13,
        "E": 14,
        "F": 15
    ]
    
    let total = hexStr.uppercased().unicodeScalars.reduce(0) { acc, elem -> UInt8 in
        return acc * 16 + hexValues[String(elem)]!
    }
    
    return total
}

let hex1 = "16"
let bin1 = hexToByte(hexStr: hex1)

/// Break a string of hex characters into pairs of hex
/// characters, each pair representing a byte.
func hexStringInPairs(hexStr: String) -> [String] {
    func adjustedString(str: String) -> String {
        let len = str.characters.count
        if len % 2 == 0 { return str }
        return "0\(str)"
    }
    
    func characterPairsIn(string: String) -> [String] {
        var i = string.startIndex
        var j = string.index(i, offsetBy: 2)
        var result = [String]()
        
        while j < string.endIndex {
            let range = Range(uncheckedBounds: (lower: i, upper: j))
            let substring = string[range]
            result.append(substring)
            
            i = j
            j = string.index(j, offsetBy: 2)
        }
        
        return result
    }
    
    let adj = adjustedString(str: hexStr)
    
    return characterPairsIn(string: adj)
}

let pairs = hexStringInPairs(hexStr: hex)

func bytesToBase64(input: [UInt8]) -> String {
    let codes = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
    func codeAtIndex(index: UInt8) -> Character {
        return codes[codes.index(codes.startIndex, offsetBy: String.IndexDistance(index))]
    }
    
    func adjustedBytes(bytes: [UInt8]) -> [UInt8] {
        var copy = bytes
        while !(copy.count % 3 == 0) {
            copy.append(0)
        }
        return copy
    }
    
    var characters = [Character]()
    
    let bytes = adjustedBytes(bytes: input)
    for index in stride(from: 0, to: bytes.count, by: 3) {
        let byte1 = bytes[index]
        let char1 = (byte1 & 0xfc) >> 2
        characters.append(codeAtIndex(index: char1))
        
        guard index + 1 < bytes.count else {
            characters.append("=")
            characters.append("=")
            continue
        }
        
        let byte2 = bytes[index+1]
        let char2 = ((byte1 & 0x03) << 4) + ((byte2 & 0xf0) >> 4)
        characters.append(codeAtIndex(index: char2))
        
        guard index + 2 < bytes.count else {
            characters.append("=")
            continue
        }
        
        let byte3 = bytes[index+2]
        let char3 = ((byte2 & 0x0f) << 2) + ((byte3 & 0xc0) >> 6)
        characters.append(codeAtIndex(index: char3))
        
        print("bytes are \(String(byte1, radix: 2)), \(String(byte2, radix: 2)), \(String(byte3, radix: 2))")
        
        let char4 = (byte3 & 0x3f)
        characters.append(codeAtIndex(index: char4))
    }
    
    let base64 = characters.map { String($0) }.joined()
    
    return base64
}

let result = bytesToBase64(input: hexStringInPairs(hexStr: hex).map(hexToByte))

print("Expected: \(expectedBase64)")
print("Got:      \(result)")

