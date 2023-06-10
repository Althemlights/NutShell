grep  "Startpoint\|Endpoint\|slack" Top_750_0.35margin.timing.rpt > timing.rpt
awk 'ORS=NR%3?" ":"\n"' timing.rpt > new_log.rpt
grep -n 'l2cache' new_log.rpt > l2cache.rpt
sed -i '/l2cache/d' new_log.rpt 


grep -n 'icache' new_log.rpt > icache.rpt
sed -i '/icache/d' new_log.rpt 

grep -n 'SSDCSR' new_log.rpt > SSDCSR.rpt
sed -i '/SSDCSR/d' new_log.rpt 

 
grep -n 'Bypass' new_log.rpt > Bypass.rpt
sed -i '/Bypass/d' new_log.rpt 

 
grep -n 'ringBufferTail' new_log.rpt > ringBufferTail.rpt
sed -i '/ringBufferTail/d' new_log.rpt 

 
grep -n 'LSU' new_log.rpt > LSU.rpt
sed -i '/LSU/d' new_log.rpt 

 
grep -n 'l3cache' new_log.rpt > l3cache.rpt
sed -i '/l3cache/d' new_log.rpt 

 
grep -n 'dcache' new_log.rpt > dcache.rpt
sed -i '/dcache/d' new_log.rpt 

 
grep -n 'MDU' new_log.rpt > MDU.rpt
sed -i '/MDU/d' new_log.rpt 

 
grep -n 'coupledPipe' new_log.rpt > coupledPipe.rpt
sed -i '/coupledPipe/d' new_log.rpt 

 
grep -n 'timer' new_log.rpt > timer.rpt
sed -i '/timer/d' new_log.rpt 

 
grep -n 'ifu' new_log.rpt > ifu.rpt
sed -i '/ifu/d' new_log.rpt 

 
grep -n 'pipeStage' new_log.rpt > pipeStage.rpt
sed -i '/pipeStage/d' new_log.rpt 

 
grep -n 'buffer_2' new_log.rpt > buffer_2.rpt
sed -i '/buffer_2/d' new_log.rpt 

 
grep -n 'axi4buf' new_log.rpt > axi4buf.rpt
sed -i '/axi4buf/d' new_log.rpt 

 
grep -n 'axi4deint' new_log.rpt > axi4deint.rpt
sed -i '/axi4deint/d' new_log.rpt 
 
grep -n 'axi4yank' new_log.rpt > axi4yank.rpt
sed -i '/axi4yank/d' new_log.rpt 

 
grep -n 'uncache' new_log.rpt > uncache.rpt
sed -i '/uncache/d' new_log.rpt 

 
grep -n 'buffer' new_log.rpt > buffer.rpt
sed -i '/buffer/d' new_log.rpt 