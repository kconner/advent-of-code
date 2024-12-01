brew install janet
jpm install spork

run:
janet 1.janet

make an image and use it:
janet -c 1.janet 1.jimage
janet -i 1.jimage

package and run an executable:
jpm build
build/1

vs code extensions:
- janet-lang.vscode-janet
- dlyanb.janet-formatter
