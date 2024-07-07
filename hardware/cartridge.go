package hardware

import (
	"log"
	"os"
)

type Cartridge struct {
	PRG_ROM            []uint8
	CHR_ROM            []uint8
	SRAM               []uint8
	MapperID           uint8
	Mirror             uint8
	Battery            uint8
	AltNameTableLayout uint8
}

func LoadCartridge(file *os.File) Cartridge {
	var cart Cartridge
	headerBuffer := make([]byte, 16)
	n, err := file.Read(headerBuffer)
	if n < 16 {
		log.Fatalf("Invalid NES file (incomplete header)")
	}
	if err != nil {
		log.Fatalf("Error reading file: %v", err)
	}
	validNESFileBytes := [4]byte{0x4e, 0x45, 0x53, 0x1a}
	for i, b := range validNESFileBytes {
		if b != headerBuffer[i] {
			log.Fatalf("Invalid NES file, bytes expected: %v bytes received: %v", validNESFileBytes, headerBuffer[0:4])
		}
	}
	cart.PRG_ROM = make([]uint8, int(headerBuffer[4])*16384)
	cart.CHR_ROM = make([]uint8, int(headerBuffer[5])*8192)
	cart.Mirror = headerBuffer[6] & 1
	cart.Battery = headerBuffer[6] & 2
	cart.AltNameTableLayout = headerBuffer[6] & 8
	cart.MapperID = (headerBuffer[7] & 0b1111_0000) | (headerBuffer[6] >> 4)
	return cart
}
