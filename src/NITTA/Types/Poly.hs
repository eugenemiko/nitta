{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-
По большому счёту выносить это объявление типа в отдельный модуль обусловлено расширением PolyKinds,
из-за которого компилятор перестают правильно воспринимать переменные типов без аннотаций и надо
существенно переписывать код.
-}
module NITTA.Types.Poly where

import           Data.Proxy

-- | Общий класс для описания варантов и решений.
--
-- Инстансы данного класса должны оканчиваться иметь суфикс DT. Например: BindingDT. Конктрукторы
-- опций должны иметь суфикс O, а решений - D.
class DecisionType (t :: *) where
  data Option t :: *
  data Decision t :: *

-- | Общий интерфейс для работы с принятия решений. Единственная проблема - он требует явного
-- прописывания Proxy типа, что в общем-то сильно колечит саму идею общего интерфейса. Может быть,
-- когда-нибудь удастся это дело поправить.
--
-- С чем связана проблема:
--
-- - Описание варианта (Option) и решения (Decision) должны специализироваться по типам. Есть три
--   способа это обеспечить: 1) Определить необходимый kind для Option и Decision, а их значения
--   явно указывать в классе Decision (не подходит, так как не понятно какой должен быть kind). 2)
--   Объявить все необходимые переменные непосредственно в типе DesicionType, значения которых будут
--   связываться при объявлении инстанса класс Decision.
-- - Если выбран вариант 2, то необъодимо согласовать между собой типы источника вариантов (s) и
--   типов ваниарта (t). Делать это вручную - очень много кода. По этому определяется функциональная
--   зависимость s -> t, которая не приводит к колизии, если один объект реализуется несколько
--   вариантов типов решений.
--
-- На текущий момент данный класс можно разбить на несколько классов, количество которых будет равно
-- количеству инстансов DecisionType. Возможный способ использования - инкапсулировать тип решения в
-- рамках процедуры принятия решения (но осмысленность данного действия не ясна).
--
-- FIXME: дублирование аргументов, а именно: dt является именем типа с произвольным kind-ом,
-- параметризованная версия которого - t. (dt :: k) если k = 1 то (t <=> dt _ :: *)
class DecisionProblem (t :: *) dt (s :: *) | dt s -> t where
  options :: Proxy dt -> s -> [Option t]
  decision :: Proxy dt -> s -> Decision t -> s
