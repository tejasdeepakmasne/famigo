package cartridge

import "github.com/tejasdeepakmasne/famigo/hardware/memory"

type Cartridge struct {
	SRAM    []uint8
	PRG_ROM []uint8
	CHR_ROM []uint8
	Mapper  byte
	Mirror  byte
	Battery byte
	Memory  memory.Memory
}
