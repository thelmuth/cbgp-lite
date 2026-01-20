# Metrics:
# - Score: 3.00/3.00
# - Lines: 7
# - Complexity: 2
# - Structure: for_only
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.55
# - Seed: 21

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = (arg2 + 0.58) / 2.91
    for i0 in range(abs(arg1) + 1):
        var0 = var0 + float(i0)
        var0 /= max(0.1, i0 / 3)
        var1 = arg0 * 1.61
    var1 += var0
    return int(var1)