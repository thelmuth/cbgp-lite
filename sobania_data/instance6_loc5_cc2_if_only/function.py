# Metrics:
# - Score: 3.00/3.00
# - Lines: 5
# - Complexity: 2
# - Structure: if_only
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.58
# - Seed: 17

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = abs(arg1)
    if arg2 <= 10:
        var0 *= (arg0 * arg0) % 5 + 3
    var0 = var0 * arg0
    return var0 * 5