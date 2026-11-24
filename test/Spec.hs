module Main where

import RIO
import Sqint.CumulativeSpec qualified as CumulativeSpec
import Sqint.DeferredDependencySpec qualified as DeferredDependencySpec
import Sqint.DeploySpec qualified as DeploySpec
import Sqint.DomainsSpec qualified as DomainsSpec
import Sqint.LockSpec qualified as LockSpec
import Sqint.OutputSpec qualified as OutputSpec
import Sqint.PgAstSpec qualified as PgAstSpec
import Sqint.PlanCheckSpec qualified as PlanCheckSpec
import Sqint.QueryPlanSpec qualified as QueryPlanSpec
import Sqint.SetLocalSpec qualified as SetLocalSpec
import Sqint.SuppressionSpec qualified as SuppressionSpec
import Test.Tasty


main :: IO ()
main =
    defaultMain
        $ testGroup
            "sqint"
            [ CumulativeSpec.tests
            , DeferredDependencySpec.tests
            , DeploySpec.tests
            , DomainsSpec.tests
            , LockSpec.tests
            , OutputSpec.tests
            , PgAstSpec.tests
            , PlanCheckSpec.tests
            , QueryPlanSpec.tests
            , SetLocalSpec.tests
            , SuppressionSpec.tests
            ]
