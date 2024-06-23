//collect all components
package hardware

type NES struct {
	cpu *CPU
	ppu *PPU
	cart *Cartridge
}