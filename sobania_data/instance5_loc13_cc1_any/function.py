# Metrics:
# - Score: 3.00/3.00
# - Lines: 13
# - Complexity: 1
# - Structure: any
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.59
# - Seed: 93

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = arg1
    var0 += arg0 % 2
    var0 += 4
    var0 //= abs(arg1 + 2) % 2 + 2
    var1 = arg0 * 1.34
    var0 -= int(var1)
    var0 += 5
    var0 *= (arg2 * arg2) % 5 + 1
    var2 = var1 * 0.9
    var0 += int(var2) % 9
    var0 -= arg1
    var0 //= 4
    return var0