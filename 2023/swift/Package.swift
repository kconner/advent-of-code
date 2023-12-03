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
    ]
)
