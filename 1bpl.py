PIC_DIR="./"

def load_bitmaps(fnames, outfile):
    import struct
    # Write out the offset table first
    for i in range(0, len(fnames)):
        offset = 4*len(fnames) + i * 8000
        outfile.write(struct.pack(">I", offset))

    # Now convert the images
    # Be sure to specify the correct image dimensions for x/y
    for fname in fnames:
        from PIL import Image
        original = Image.open(PIC_DIR + fname)

        bits = bytearray()
        for y in range(0,384):
            acc = 0
            for x in range(0,120):
                p = original.getpixel((x,y)) & 1
                #assert p < 2, "p is %d" % p
                acc <<= 1
                acc |= p
                if (x&7 == 7):
                    bits.append(acc)
                    acc = 0
        outfile.write(bits)

fnames = []
fnames.append("select2.png")
outfile = open("select2.dat", "wb")
load_bitmaps(fnames, outfile)
outfile.close()
