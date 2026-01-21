# Metrics:
# - Score: 3.00/3.00
# - Lines: 10
# - Complexity: 3
# - Structure: if_in_for
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.57
# - Seed: 173

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = 2
    for i0 in range(abs(arg2) + 1):
        var1 = 1
        if i0 % 2 == 0:
            var1 *= 2
        var0 += var1
    var0 -= arg1 // 5
    var0 -= abs(arg0)
    var0 = abs(var0)
    return var0