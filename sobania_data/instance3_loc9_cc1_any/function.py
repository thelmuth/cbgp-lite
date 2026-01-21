# Metrics:
# - Score: 3.00/3.00
# - Lines: 9
# - Complexity: 1
# - Structure: any
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.60
# - Seed: 35

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = arg2
    var0 *= 4
    var0 *= 2 - abs(arg2) % 5
    var0 += arg1 * -4
    var1 = var0 * 1.36
    var0 *= max(1, int(abs(var1)) % 5)
    var0 //= 5 - abs(arg0) % 5
    var0 -= arg2 // 2
    return var0