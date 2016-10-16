//: [Previous](@previous)

import Foundation

// Write a function that takes two equal-length buffers and produces their XOR combination.
// If your function works properly, then when you feed it the string:
let input = "1c0111001f010100061a024b53535009181c"
// ... after hex decoding, and when XOR'd against:
let other = "686974207468652062756c6c277320657965"
// ... should produce:
let expected = "746865206b696420646f6e277420706c6179"

let decodedLHS = hexDecode(input)
let decodedRHS = hexDecode(other)

let paired = zip(decodedLHS, decodedRHS)
let result = paired.map { $0 ^ $1 }

if hexEncode(result) == expected {
    print("SUCCESS!!!")
}

//: [Next](@next)
