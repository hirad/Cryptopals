//: Playground - noun: a place where people can play

import UIKit

// Set 1, challenge 1
// Convert hex to base64
// This input...
let hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
// should produce...
let expectedBase64 = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

// given a byte, gives 8-character string representation (for debugging)
private func byteString(_ byte: UInt8) -> String {
    var str = String(byte, radix: 2)
    let length = str.characters.count
    
    for _ in 0..<(8 - length) {
        str = "0" + str
    }
    
    return str
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
        let last = string.index(string.endIndex, offsetBy: -2)
        var result = [String]()
        
        while i <= last {
            let j = string.index(i, offsetBy: 2)
            
            let range = Range(uncheckedBounds: (lower: i, upper: j))
            let substring = string[range]
            result.append(substring)
            
            i = j
        }
        
        return result
    }
    
    let adj = adjustedString(str: hexStr)
    
    return characterPairsIn(string: adj)
}

let pairs = hexStringInPairs(hexStr: hex)

func hexDecode(_ str: String) -> [UInt8] {
    return hexStringInPairs(hexStr: str).map(hexToByte)
}

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
        let idx1 = (byte1 & 0xfc) >> 2
        let code1 = codeAtIndex(index: idx1)
        characters.append(code1)
        
        guard index + 1 < bytes.count else {
            characters.append("=")
            characters.append("=")
            continue
        }
        
        let byte2 = bytes[index+1]
        let idx2 = ((byte1 & 0x03) << 4) + ((byte2 & 0xf0) >> 4)
        let code2 = codeAtIndex(index: idx2)
        characters.append(code2)
        
        guard index + 2 < bytes.count else {
            characters.append("=")
            continue
        }
        
        let byte3 = bytes[index+2]
        let idx3 = ((byte2 & 0x0f) << 2) + ((byte3 & 0xc0) >> 6)
        let code3 = codeAtIndex(index: idx3)
        characters.append(code3)
        
        let idx4 = (byte3 & 0x3f)
        let code4 = codeAtIndex(index: idx4)
        characters.append(code4)
    }
    
    let base64 = characters.map { String($0) }.joined()
    
    return base64
}

let result = bytesToBase64(input: hexDecode(hex))

print("Expected: \(expectedBase64)")
print("Got:      \(result)")

