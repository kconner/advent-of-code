// swift-tools-version: 5.9

import PackageDescription

let package = Package(
    name: "2",
    platforms: [
        .macOS(.v14)
    ],
    products: [
        .executable(
            name: "2",
            targets: ["2"]
        )
    ],
    dependencies: [
        .package(
            url: "https://github.com/pointfreeco/swift-parsing",
            exact: "0.13.0"
        ),
    ],
    targets: [
        .executableTarget(
            name: "2",
            dependencies: [
                .product(name: "Parsing", package: "swift-parsing"),
            ]
        )
    ]
)
