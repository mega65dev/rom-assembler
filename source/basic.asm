;
;	THIS FILE IS AUTOMATICALLY GENERATED
;
!source "system/header.asm"
!source "data/constants.asm"
!source "data/zeropage.asm"
!source "system/stackdata.asm"
!source "data/memory.asm"
!source "data/kernal.asm"
!source "initialise/initialise.asm"
!source "system/macros.asm"
!source "system/indirection.asm"
!source "system/tokeniser.asm"
!source "tokeniser/keywords.asm"
!source "tokeniser/keyword/esc.asm"
!source "tokeniser/vectors.asm"
!source "tokeniser/const.asm"
!source "error/messages.asm"
!source "error/constants.asm"
!source "error/message.asm"
!source "system/dispatcher.asm"
!source "command/set1.asm"
!source "function/dispatch.asm"
!source "operator/binary.asm"
!source "operator/relational.asm"
!source "system/readyerror.asm"
!source "error/handler.asm"
!source "system/interface.asm"
!source "system/execute.asm"
!source "edit/shift.asm"
!source "system/linkprogram.asm"
!source "command/input/handler.asm"
!source "system/stack.asm"
!source "system/linesearch.asm"
!source "command/text/get.asm"
!source "command/list.asm"
!source "command/list/basic.asm"
!source "command/newclr.asm"
!source "stack/init.asm"
!source "command/return.asm"
!source "command/data.asm"
!source "command/ifthenelse.asm"
!source "command/on.asm"
!source "command/let/standard.asm"
!source "command/text,print.asm"
!source "command/text/input.asm"
!source "command/next.asm"
!source "command/dim.asm"
!source "command/sys.asm"
!source "command/dma.asm"
!source "command/trace.asm"
!source "command/sys/returnreg.asm"
!source "command/let/midstring.asm"
!source "command/auto.asm"
!source "command/gotosub.asm"
!source "command/continue.asm"
!source "command/run.asm"
!source "command/restore.asm"
!source "command/renumber.asm"
!source "command/for.asm"
!source "command/delete.asm"
!source "command/findchange.asm"
!source "command/trap.asm"
!source "command/resume.asm"
!source "command/loops.asm"
!source "command/key.asm"
!source "command/bank.asm"
!source "command/sound/play.asm"
!source "command/sound/filter.asm"
!source "command/sound/envelope.asm"
!source "command/sound/volume.asm"
!source "command/sound/sound.asm"
!source "command/text/window.asm"
!source "command/fastslow.asm"
!source "system/evaluate.asm"
!source "system/arrays.asm"
!source "command/time.asm"
!source "system/time.asm"
!source "command/sleep.asm"
!source "command/wait.asm"
!source "function/fre.asm"
!source "function/val.asm"
!source "function/dec.asm"
!source "command/peekpoke.asm"
!source "function/errstr.asm"
!source "function/hexstr.asm"
!source "function/joy.asm"
!source "function/potpen.asm"
!source "function/pointer.asm"
!source "operator/xor.asm"
!source "operator/mod.asm"
!source "function/rwindow.asm"
!source "function/rnd.asm"
!source "math/utils.asm"
!source "function/userdef.asm"
!source "function/stringmisc.asm"
!source "string/manager.asm"
!source "string/garbage.asm"
!source "string/garbage/utils.asm"
!source "math/parameters.asm"
!source "math/addsub.asm"
!source "math/const.asm"
!source "math/log.asm"
!source "math/multiply.asm"
!source "math/unpack.asm"
!source "math/integer.asm"
!source "math/fpin.asm"
!source "math/convert.asm"
!source "math/logarithms.asm"
!source "math/polyeval.asm"
!source "math/trigonometry.asm"
!source "system/boot.asm"
!source "command/text/printusing.asm"
!source "command/fform.asm"
!source "function/instr.asm"
!source "function/type.asm"
!source "command/disk.asm"
!source "dos/setup.asm"
!source "command/ldir.asm"
!source "function/dopen.asm"
!source "command/bload.asm"
!source "command/header.asm"
!source "dos/errors.asm"
!source "command/scratch.asm"
!source "command/record.asm"
!source "command/dclear.asm"
!source "command/collect.asm"
!source "command/copy.asm"
!source "command/concat.asm"
!source "command/rename.asm"
!source "command/trans.asm"
!source "dos/parser.asm"
!source "dos/sendparam.asm"
!source "handler/irq.asm"
!source "command/mouse.asm"
!source "function/rmouse.asm"
!source "command/cursor.asm"
!source "function/rcursor.asm"
!source "command/graphics/screen.asm"
!source "command/graphics/pen.asm"
!source "command/graphics/dmode.asm"
!source "command/graphics/dpat.asm"
!source "command/graphics/palette.asm"
!source "command/graphics/line.asm"
!source "command/graphics/box.asm"
!source "command/graphics/circle.asm"
!source "command/graphics/ellipse.asm"
!source "command/graphics/polygon.asm"
!source "command/graphics/set.asm"
!source "command/graphics/char.asm"
!source "command/graphics/paint.asm"
!source "command/graphics/loadiff.asm"
!source "command/graphics/saveiff.asm"
!source "command/graphics/viewport.asm"
!source "command/graphics/genlock.asm"
!source "command/graphics/color.asm"
!source "command/graphics/sprite.asm"
!source "command/graphics/movspr.asm"
!source "command/graphics/sprcor.asm"
!source "command/graphics/sprcolor.asm"
!source "command/graphics/collision.asm"
!source "function/graphics/rcolor.asm"
!source "function/graphics/rgraphic.asm"
!source "function/graphics/pixel.asm"
!source "function/graphics/rpen.asm"
!source "function/graphics/rpalette.asm"
!source "function/graphics/rsprite.asm"
!source "function/graphics/rsppos.asm"
!source "function/graphics/bump.asm"
!source "edit/mode.asm"
!source "initialise/sprites.asm"
!source "handler/nmi.asm"
!source "system/jumptable.asm"
