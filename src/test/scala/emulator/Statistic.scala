
import spire.math.UInt
import java.io.File
import java.io.PrintWriter
import chiseltest._

class Statistic {
    private var imgName = ""
    private var reportPath = ""
    private var totalCycles = 0
    private var totalInsts  = 0

    private var totalBranch     = 0
    private var totalBranchFail = 0
    private var totalCall       = 0
    private var totalCallFail   = 0
    private var totalRet        = 0
    private var totalRetFail    = 0
    
    private var totalICacheVisit = 0
    private var totalICacheHit  = 0
    private var totalDCacheRVisit = 0
    private var totalDCacheRHit  = 0
    private var totalDCacheWVisit = 0
    private var totalDCacheWHit  = 0
    private var totalL2ICacheVisit = 0
    private var totalL2ICacheHit  = 0
    private var totalL2DCacheVisit = 0
    private var totalL2DCacheHit  = 0

    private var totalICacheMissCycle = 0
    private var totalFQFullCycle = 0
    private var totalFQEmptyCycle = 0
    private var totalRnmFListEmptyCycle = 0

    private var totalROBFullCycle = 0
    private var totalBDBFullCycle = 0

    private var totalArIQFullCycle = 0
    private var totalMdIQFullCycle = 0
    private var totalLsIQFullCycle = 0
    private var totalDivBusyCycle = 0
    private var totalDCacheMissCycle = 0
    private var totalSBFullCycle = 0

    private var totalALUInsts = 0
    private var totalBranchInsts = 0
    private var totalLoadInsts = 0
    private var totalStoreInsts = 0
    private var totalMulInsts = 0
    private var totalDivInsts = 0
    

    def setImgName(name: String): Unit = {
        imgName = name
    }
    def setReportPath(testRunDir: String, name: String): Unit = {
        val reportDirPath = testRunDir + "/reports"
        val reportDir = new File(reportDirPath)
        if(!reportDir.exists()){
            reportDir.mkdirs()
        }
        reportPath = reportDirPath + "/" + name + ".md"
    }

    def addCycles(cycles: Int): Unit = {
        totalCycles += cycles
    }

    def addInsts(insts: Int): Unit = {
        totalInsts += insts
    }

    def addJump(branch: Int, call: Int, ret: Int, branchFail: Int, callFail: Int, retFail: Int): Unit = {
        totalBranch     += branch
        totalCall       += call
        totalRet        += ret
        totalBranchFail += branchFail
        totalCallFail   += callFail
        totalRetFail    += retFail
    }

    def addCacheVisit(
        icVisit: Int, icHit: Int, 
        dcRVisit: Int, dcRHit: Int, 
        dcWVisit: Int, dcWHit: Int,
        l2icVisit: Int, l2icHit: Int,
        l2dcVisit: Int, l2dcHit: Int
    ): Unit = {
        totalICacheVisit  += icVisit
        totalICacheHit    += icHit
        totalDCacheRVisit += dcRVisit
        totalDCacheRHit   += dcRHit
        totalDCacheWVisit += dcWVisit
        totalDCacheWHit   += dcWHit
        totalL2ICacheVisit += l2icVisit
        totalL2ICacheHit   += l2icHit
        totalL2DCacheVisit += l2dcVisit
        totalL2DCacheHit   += l2dcHit
    }
    def addFrontendBlockCycle(cpu: CPU): Unit = {
        totalICacheMissCycle    += cpu.io.dbg.get.fte.ic.missCycle.peek().litValue.toInt
        totalFQFullCycle        += cpu.io.dbg.get.fte.fq.fullCycle.peek().litValue.toInt
        totalFQEmptyCycle       += cpu.io.dbg.get.fte.fq.emptyCycle.peek().litValue.toInt
        totalRnmFListEmptyCycle += cpu.io.dbg.get.fte.rnm.fList.fListEmptyCycle.peek().litValue.toInt
    }

    def addDispatchBlockCycle(cpu: CPU): Unit = {
        totalROBFullCycle += cpu.io.dbg.get.cmt.rob.fullCycle.peek().litValue.toInt
        totalBDBFullCycle += cpu.io.dbg.get.cmt.bdb.fullCycle.peek().litValue.toInt
    }

    def addBackendBlockCycle(cpu: CPU): Unit = {
        totalArIQFullCycle      += cpu.io.dbg.get.bke.arIQ.fullCycle.peek().litValue.toInt
        totalMdIQFullCycle      += cpu.io.dbg.get.bke.mdIQ.fullCycle.peek().litValue.toInt
        totalLsIQFullCycle      += cpu.io.dbg.get.bke.lsIQ.fullCycle.peek().litValue.toInt
        totalDivBusyCycle       += cpu.io.dbg.get.bke.mdPP.srt2.busyCycle.peek().litValue.toInt
        totalDCacheMissCycle    += cpu.io.dbg.get.bke.lsPP.dc(0).asInstanceOf[DCacheReadDBG].missCycle.peek().litValue.toInt
        totalSBFullCycle        += cpu.io.dbg.get.bke.lsPP.dc(0).asInstanceOf[DCacheReadDBG].sbFullCycle.peek().litValue.toInt
    }

    def getTotalCycles(): Int = {
        totalCycles
    }

    def getTotalInsts(): Int = {
        totalInsts
    }

    def getIpc(): Double = {
        // 保留6位小数
        BigDecimal(totalInsts.toDouble / totalCycles.toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
    }

    // def getJump(): (Int, Int, Int) = {
    //     (totalBranch, totalCall, totalRet)
    // }

    // def getBranchSuccess(): (Int, Int, Int) = {
    //     (totalBranch - totalBranchFail, totalCall - totalCallFail, totalRet - totalRetFail)
    // }
    def addInstNums(iNum: InstNum): Unit = {
        totalALUInsts += iNum.alu
        totalBranchInsts += iNum.branch
        totalLoadInsts += iNum.load
        totalStoreInsts += iNum.store
        totalMulInsts += iNum.mul
        totalDivInsts += iNum.div
    }
    def makeMarkdownReport(): Unit = {
        // 写模式打开reportPath
        val writer = new PrintWriter(new File(reportPath))
        // 写入标题
        writer.write(s"# Zircon运行报告\n")
        // 写入统计信息
        writer.write(s"## 程序基本情况\n")
        // 插入一个4列两行的表格，第一列是程序名，第二列是总周期数，第三列是总指令数，第四列是IPC
        writer.write(s"| 程序名 | 总周期数 | 总指令数 | IPC |\n")
        writer.write(s"| --- | --- | --- | --- |\n")
        writer.write(s"| ${imgName} | ${totalCycles} | ${totalInsts} | ${getIpc()} |\n")

        // 写入指令统计信息
        writer.write(s"### 指令统计\n")
        writer.write(s"| 指令类型 | 总数 | 占比 |\n")
        writer.write(s"| --- | --- | --- |\n")
        writer.write(s"| ALU | ${totalALUInsts} | ${BigDecimal(totalALUInsts.toDouble / totalInsts.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| Branch | ${totalBranchInsts} | ${BigDecimal(totalBranchInsts.toDouble / totalInsts.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| Load | ${totalLoadInsts} | ${BigDecimal(totalLoadInsts.toDouble / totalInsts.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| Store | ${totalStoreInsts} | ${BigDecimal(totalStoreInsts.toDouble / totalInsts.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| Mul | ${totalMulInsts} | ${BigDecimal(totalMulInsts.toDouble / totalInsts.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| Div | ${totalDivInsts} | ${BigDecimal(totalDivInsts.toDouble / totalInsts.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        
        // 分支预测

        val BranchSuccess       = totalBranch - totalBranchFail
        val BranchSuccessRate   = if(totalBranch != 0) BigDecimal((totalBranch - totalBranchFail).toDouble / totalBranch.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble else 0.0
        val CallSuccess         = totalCall - totalCallFail
        val CallSuccessRate     = if(totalCall != 0) BigDecimal((totalCall - totalCallFail).toDouble / totalCall.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble else 0.0
        val ReturnSuccess       = totalRet - totalRetFail
        val ReturnSuccessRate   = if(totalRet != 0) BigDecimal((totalRet - totalRetFail).toDouble / totalRet.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble else 0.0
        
        writer.write(s"## 分支预测\n")
        writer.write(s"| 分支类型 | 总数 | 预测正确数 | 预测正确率 |\n")
        writer.write(s"| --- | --- | --- | --- |\n")
        writer.write(s"| Branch | ${totalBranch} | ${BranchSuccess} | ${BranchSuccessRate}% |\n")
        writer.write(s"| Call | ${totalCall} | ${CallSuccess} | ${CallSuccessRate}% |\n")
        writer.write(s"| Return | ${totalRet} | ${ReturnSuccess} | ${ReturnSuccessRate}% |\n")

        // 高速缓存命中率
        val ICacheSuccessRate       = if(totalICacheVisit != 0) BigDecimal(totalICacheHit.toDouble / totalICacheVisit.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble else 0.0
        val DCacheReadSuccessRate   = if(totalDCacheRVisit != 0) BigDecimal(totalDCacheRHit.toDouble / totalDCacheRVisit.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble else 0.0
        val DCacheWriteSuccessRate  = if(totalDCacheWVisit != 0) BigDecimal(totalDCacheWHit.toDouble / totalDCacheWVisit.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble else 0.0
        val L2ICacheSuccessRate     = if(totalL2ICacheVisit != 0) BigDecimal(totalL2ICacheHit.toDouble / totalL2ICacheVisit.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble else 0.0
        val L2DCacheSuccessRate     = if(totalL2DCacheVisit != 0) BigDecimal(totalL2DCacheHit.toDouble / totalL2DCacheVisit.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble else 0.0

        writer.write(s"## 高速缓存命中\n")
        writer.write(s"| 高速缓存通道 | 访问次数 | 命中数 | 命中率 |\n")
        writer.write(s"| --- | --- | --- | --- |\n")
        writer.write(s"| ICache Read | ${totalICacheVisit} | ${totalICacheHit} | ${ICacheSuccessRate}% |\n")
        writer.write(s"| DCache Read | ${totalDCacheRVisit} | ${totalDCacheRHit} | ${DCacheReadSuccessRate}% |\n")
        writer.write(s"| DCache Write | ${totalDCacheWVisit} | ${totalDCacheWHit} | ${DCacheWriteSuccessRate}% |\n")
        writer.write(s"| L2Cache ICache | ${totalL2ICacheVisit} | ${totalL2ICacheHit} | ${L2ICacheSuccessRate}% |\n")
        writer.write(s"| L2Cache DCache | ${totalL2DCacheVisit} | ${totalL2DCacheHit} | ${L2DCacheSuccessRate}% |\n")

        writer.write(s"## 流水线停顿\n")
        writer.write(s"### 前端\n")
        writer.write(s"| 停顿原因 | 停顿周期数 | 停顿率 |\n")
        writer.write(s"| --- | --- | --- |\n")
        writer.write(s"| ICache缺失 | ${totalICacheMissCycle} | ${BigDecimal(totalICacheMissCycle.toDouble / totalCycles.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| 发射队列满 | ${totalFQFullCycle} | ${BigDecimal(totalFQFullCycle.toDouble / totalCycles.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| 发射队列空 | ${totalFQEmptyCycle} | ${BigDecimal(totalFQEmptyCycle.toDouble / totalCycles.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| 无空闲物理寄存器 | ${totalRnmFListEmptyCycle} | ${BigDecimal(totalRnmFListEmptyCycle.toDouble / totalCycles.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        
        writer.write(s"### 调度\n")
        writer.write(s"| 停顿原因 | 停顿周期数 | 停顿率 |\n")
        writer.write(s"| --- | --- | --- |\n")
        writer.write(s"| 重排序缓存满 | ${totalROBFullCycle} | ${BigDecimal(totalROBFullCycle.toDouble / totalCycles.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| 分支数据缓存满 | ${totalBDBFullCycle} | ${BigDecimal(totalBDBFullCycle.toDouble / totalCycles.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")

        writer.write(s"### 后端\n")
        writer.write(s"| 停顿原因 | 停顿周期数 | 停顿率 |\n")
        writer.write(s"| --- | --- | --- |\n")
        // 保留6位小数
        writer.write(s"| 算数发射队列满 | ${totalArIQFullCycle} | ${BigDecimal(totalArIQFullCycle.toDouble / totalCycles.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| 乘除发射队列满 | ${totalMdIQFullCycle} | ${BigDecimal(totalMdIQFullCycle.toDouble / totalCycles.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| 访存发射队列满 | ${totalLsIQFullCycle} | ${BigDecimal(totalLsIQFullCycle.toDouble / totalCycles.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| 除法器运算 | ${totalDivBusyCycle} | ${BigDecimal(totalDivBusyCycle.toDouble / totalCycles.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| DCacahe缺失 | ${totalDCacheMissCycle} | ${BigDecimal(totalDCacheMissCycle.toDouble / totalCycles.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        writer.write(s"| 写缓存满 | ${totalSBFullCycle} | ${BigDecimal(totalSBFullCycle.toDouble / totalCycles.toDouble * 100).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}% |\n")
        
        writer.close()
    }

}