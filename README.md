# ⚙️​ Huffman Compression algorithm
> A class project built entirely in **Ada**.  
> 💾​ Compress any binary file in one command, gaining up to 70% space in the process.
---


## 📌 Project Overview
Simply compress any binary file into a .hff file taking up way less space in your system.
When needed, the provided decompressor will help you extract your file as a .d file, without
any loss of information. The provided code also gives you access to a text to binary and binary
to text converter to help you ease your use of the compressor.

---


## 🚀 Features
| Feature                  | Notes                             |
|--------------------------|-----------------------------------|
| Text to binary converter | Easily convert any text file      |
| Binary to text converter | And also any binary file, to text |
| File compressor          | Compress and save storage space   |
| File decompressor        | While keeping total access to your files |
---

## ▶️ Running the Program
```bash
# Compile
gnatmake -gnatwa compresser.adb
gnatmake -gnatwa decompresser.adb
gnatmake -gnatwa binary_to_text.adb
gnatmake -gnatwa text_to_binary.adb

# Run
./text_to_binary	./testfiles/exemple.txt
./compresser 	./testfiles/exempleBINARY.txt
./decompresser 	./testfiles/exempleBINARY.txt.hff
./binary_to_text 	./testfiles/exempleBINARY.txt.hff.d
```
>💡 Make sure you have already installed GNAT Community !
---

## 🤝 Contributors
| Names        | Role           | Github Profile|
|-------------|----------------|---------------|
| *Hocine Mediani* | 👨‍💻 Developer | [> hocinemediani](https://github.com/hocinemediani) |
| *Ilian Kraifi* | 👨‍💻 Developer   | [> iki389](https://github.com/ik389) |
---


## 📜 License
This project is open-source under the [MIT License](LICENSE).

---

## 💬 Feedback & Contact
📬 Found a bug? Have suggestions?  
Open an issue or contact me directly via [hocine.mediani7@gmail.com](mailto:hocine.mediani7@gmail.com)

---
