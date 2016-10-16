import Foundation

// When a challenge is completed, its code may be used by later challenges.
// The code that requires reuse is moved in here for access by all playground
// files and the internal implementation details are abstracted.


public func hexEncode(_ byteArray: [UInt8]) -> String {
    return byteArray.map { String($0, radix: 16) }.joined()
}

/// Given two hex strings of the same length, returns a hex
/// string that is the XOR of the two.
/// (from Set 1, Challenge 2)
public func xor(lhs: String, rhs: String) -> String {
    return hexEncode(xor(hexDecode(lhs), hexDecode(rhs)))
}

/// Given two buffers of equal length, returns a buffer that
/// is the XOR of the two.
/// (from Set 1, Challenge 2)
public func xor(_ lhs: [UInt8], _ rhs: [UInt8]) -> [UInt8] {
    guard lhs.count == rhs.count else { fatalError("Can't xor two buffers of different lengths") }
    
    return zip(lhs, rhs).map { $0 ^ $1 }
}

/// Given a string of hexadecimal characters, returns an
/// array of bytes represented by that string.
/// (from Set 1, Challenge 1)
public func hexDecode(_ str: String) -> [UInt8] {
    return hexStringInPairs(hexStr: str).map(hexToByte)
}

private func hexToByte(hexStr: String) -> UInt8 {
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

/// Break a string of hex characters into pairs of hex
/// characters, each pair representing a byte.
private func hexStringInPairs(hexStr: String) -> [String] {
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

