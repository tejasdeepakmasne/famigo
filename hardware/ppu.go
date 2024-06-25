package hardware

type PPU struct {
	ctrl         uint8
	mask         uint8
	status       uint8
	oamAddr      uint8
	oamData	  uint8
	scrollX      uint8
	scrollY      uint8
	vramAddr     uint16
	tramAddr     uint16
	bufferedData uint8

	//PPU Memory
	patternTable [0x2000]uint8 // 8KB
	nameTable    [0x0800]uint8 // 2KB
	palette      [0x0020]uint8 // 32B

	oam [256]uint8 //Object Attribute Memory

	frameBuffer [240][256]uint8 // Frame buffer

	//Scanline and cycle counters
	scanline int
	cycle    int

	frameComplete bool
}

func (p *PPU) Read(address uint16) uint8 {
	switch {
		case address < 0x2000:
			return p.patternTable[address]
		case address < 0x3F00:
			return p.nameTable[address % 0x0800]
		case address < 0x4000:
			return p.palette[address % 0x0020]
		default:
			return 0
	}
}

func (p * PPU) Write(address uint16, value uint8) {
	switch {
		case address < 0x2000:
			p.patternTable[address] = value
		case address < 0x3F00:
			p.nameTable[address % 0x0800] = value
		case address < 0x4000:
			p.palette[address % 0x0020] = value
	}
}

func (p *PPU) ReadRegister(address uint16) uint8 {
	switch address {
		case 0x2002:
			data := p.status
			p.status &= ^uint8(0x80) // Clear the VBlank flag
			return data
		case 0x2007:
			value := p.bufferedData
			p.bufferedData = p.Read(p.vramAddr)
			p.vramAddr += 1 
			return value
		default:
			return 0
	}
}

func (p *PPU) WriteRegister(address uint16, value uint8) {
	switch address {
		case 0x2000:
			p.ctrl = value
		case 0x2001:
			p.mask = value
		case 0x2003:
			p.oamAddr = value
		case 0x2004:
			p.oam[p.oamAddr] = value
			p.oamAddr += 1
		case 0x2005:
			if p.scrollX == 0 {
				p.scrollX = value
			} else {
				p.scrollY = value
			}
		case 0x2006:
			if p.vramAddr == 0 {
				p.vramAddr = uint16(value) << 8
			} else {
				p.vramAddr |= uint16(value)
			}
		case 0x2007:
			p.Write(p.vramAddr, value)
			p.vramAddr += 1
		}
	}

func (p *PPU) renderBackground(scanline int) {
	for tileX := 0; tileX < 32; tileX++ {
		tileIndex := p.nameTable[(scanline/8)*32+tileX]
		tile := p.getTile(tileIndex)

		for pixelX := 0; pixelX < 8; pixelX++ {
			colorIndex := tile[scanline%8][pixelX]
			color := p.getColor(colorIndex)
			x:= tileX*8 + pixelX
			p.frameBuffer[scanline][x] = color
		}
	}
}

func (p *PPU) getTile(index uint8) [8][8]uint8 {
	baseAddress := uint16(index) * 16
	var tile [8][8]uint8

	for y := 0; y < 8; y++ {
		lowByte := p.patternTable[baseAddress+uint16(y)]
		highByte := p.patternTable[baseAddress+uint16(y)+8]

		for x := 0; x < 8; x++ {
			lowBit := (lowByte >> (7 - x)) & 1
			highBit := (highByte >> (7 - x)) & 1
			tile[x][y] = uint8(highBit << 1) | lowBit
		}
	}
	return tile
}

func (p *PPU) getColor(index uint8) uint8 {
	return p.palette[index % 0x20]
}

func (p *PPU) renderSprites (scanline int) {
	for i := 0; i < 64; i++ {
		spriteY := p.oam[i*4]
		if int(spriteY) <= scanline && scanline < int(spriteY) + 8 {
			spriteIndex := p.oam[i*4+1]
			spriteTile := p.getTile(spriteIndex)

			for pixelX := 0; pixelX < 8; pixelX++ {
				colorIndex := spriteTile[pixelX][scanline-int(spriteY)]
				color := p.getColor(colorIndex)
				x := p.oam[i*4+3] + uint8(pixelX)
				p.frameBuffer[scanline][x] = color
			}
	}
}
}

func (p *PPU) Step(cycles int) {
	for i := 0; i < cycles; i++ {
		p.cycle++
		if p.cycle == 341 {
			p.cycle = 0
			p.scanline++
			if p.scanline == 261 {
				p.scanline = -1
				p.frameComplete = true
			}
		}
		if p.scanline >= 0 && p.scanline < 240 {
			p.renderBackground(p.scanline)
			p.renderSprites(p.scanline)
		} else if p.scanline == 240 {
			p.status |= 0x80
		}
	}
}

func NewPPU() *PPU {
	return &PPU{}
}



