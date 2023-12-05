// swift-tools-version: 5.9

import PackageDescription

let package = Package(
    name: "advent-of-code-2023-swift",
    platforms: [
        .macOS(.v14)
    ],
    products: [
        .executable(name: "2", targets: ["Day2"]),
        .executable(name: "3", targets: ["Day3"]),
        .executable(name: "4", targets: ["Day4"]),
        .executable(name: "5", targets: ["Day5"]),
    ],
    dependencies: [
        .package(
            url: "https://github.com/pointfreeco/swift-parsing",
            exact: "0.13.0"
        ),
    ],
    targets: [
        .executableTarget(
            name: "Day2",
            dependencies: [
                .product(name: "Parsing", package: "swift-parsing"),
            ]
        ),
        .executableTarget(
            name: "Day3"
        ),
        .executableTarget(
            name: "Day4",
            dependencies: [
                .product(name: "Parsing", package: "swift-parsing"),
            ]
        ),
        .executableTarget(
            name: "Day5",
            dependencies: [
                .product(name: "Parsing", package: "swift-parsing"),
            ]
        ),
    ]
)
