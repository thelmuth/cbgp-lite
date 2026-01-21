# Metrics:
# - Score: 3.00/3.00
# - Lines: 11
# - Complexity: 2
# - Structure: if_only
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.69
# - Seed: 96

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = arg1
    if arg1 == 5:
        var0 = arg0 * var0
    var0 *= abs(arg2 + 3) % 2 + 3
    var2 = var0 + arg1
    var0 *= 5
    var0 = var0 + arg2
    var0 -= var2 * var2 % 2
    var0 -= 2
    var0 -= arg0 * -4
    return var0