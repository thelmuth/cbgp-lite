# Metrics:
# - Score: 3.00/3.00
# - Lines: 11
# - Complexity: 1
# - Structure: any
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.60
# - Seed: 33

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = arg2
    var0 *= 4 - abs(arg2) % 5
    var0 //= 4
    var0 -= arg0 * 2
    var0 *= abs(arg1 + 3) % 2 + 1
    var1 = var0 * 1.11
    var0 *= max(1, int(abs(var1)) % 4)
    var0 *= 4 - abs(arg0) % 3
    var0 *= 4 - abs(arg1) % 4
    var0 *= 3
    return var0