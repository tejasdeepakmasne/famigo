// file to consolidate all the parts of hardware
package console

import (
	"github.com/tejasdeepakmasne/famigo/hardware/cartridge"
	"github.com/tejasdeepakmasne/famigo/hardware/cpu"
	"github.com/tejasdeepakmasne/famigo/hardware/ppu"
)

type Console struct {
	CPU       *cpu.CPU
	Cartridge *cartridge.Cartridge
	PPU       *ppu.PPU
}
