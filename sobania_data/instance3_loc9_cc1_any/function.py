# Metrics:
# - Score: 3.00/3.00
# - Lines: 9
# - Complexity: 1
# - Structure: any
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.60
# - Seed: 32

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = arg0
    var1 = arg2 / 4.16
    var0 += int(var1) % 10
    var0 -= abs(arg1)
    var2 = var1 - 1.59
    var0 *= max(1, int(abs(var2)) % 4)
    var0 *= 2
    var0 //= (arg0 * arg0) % 2 + 1
    return var0