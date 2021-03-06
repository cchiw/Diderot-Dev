cd src/compiler
git add basis/*.sml
git add translate/*.sml
git add ast/*.sml
git add ein/*.sml
git add ein/sources.cm
git add gen/ir/*.spec
git add high-ir/*.sml
git add high-opt/*.sml
git add high-to-mid/*.sml
git add high-to-mid/sources.cm
git add mid-ir/*.sml
git add mid-to-low/*.sml
git add options/ctl.sml
git add cxx-util/*.sml
git add codegen/sources.cm
git add driver/*.sml
git add simple/*.sml
git add simplify/*.sml
git add target-cpu/*.sml
git add typechecker/*.sml
git add simple-opt/*.sml
git add parser/*.sml
git add parser/*.grm
git add parse-tree/*.sml
git add mid-opt/*.sml
git add cfg-ir/*.sml
rm */*.o
rm */*/*.o
rm */*/*/*.o
rm -r */.cm/
rm -r */*/.cm/
rm -r */*/*/.cm/
cd ..
cd ..
git rm lib/*.o
