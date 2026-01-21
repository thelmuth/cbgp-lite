# Metrics:
# - Score: 3.00/3.00
# - Lines: 10
# - Complexity: 3
# - Structure: nested_for
# - LineRelevance: 1.00
# - InputRelevance: 1.00
# - LineCoverage: 1.00
# - OutputVariability: 0.77
# - Seed: 107

def generated_func(arg0: int, arg1: int, arg2: int) -> int:
    var0 = 1
    for i0 in range(abs(arg2) + 1):
        var1 = arg0 + arg1
        for i1 in range(abs(i0) + 1):
            var1 += i0 * i1
        var0 += var1
    var0 += 2
    var0 -= arg0 - 3
    var0 *= 2
    return var0