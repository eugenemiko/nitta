{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Module      : NITTA.XMILEFrontend
Description : XMILE frontend prototype
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.XMILEFrontend (
    parseXMILE,
) where

import Data.String.Interpolate
import Data.Text.Encoding (encodeUtf8)
import Xeno.DOM

xmileSample =
    [__i| 
                <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
                <xmile version="1.0" xmlns="http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
                    <header>
                        <vendor>James Houghton</vendor>
                        <name>Teacup</name>
                        <options>
                            <uses_outputs/>
                        </options>
                        <product version="1.0">Hand Coded XMILE</product>
                    </header>
                    <sim_specs>
                        <stop>30.0</stop>
                        <start>0.0</start>
                        <dt>0.125</dt>
                    </sim_specs>
                    <model>
                        <variables>
                            <flow name="Heat Loss to Room">
                                <doc>Heat Loss to Room</doc>
                                <eqn>("Teacup Temperature"-"Room Temperature")/"Characteristic Time"</eqn>
                            </flow>
                            <aux name="Room Temperature">
                                <doc>Ambient Room Temperature</doc>
                                <eqn>70</eqn>
                            </aux>
                            <stock name="Teacup Temperature">
                                <doc>The average temperature of the tea and the cup</doc>
                                <outflow>"Heat Loss to Room"</outflow>
                                <eqn>180</eqn>
                            </stock>
                            <aux name="Characteristic Time">
                                <eqn>10</eqn>
                            </aux>
                        </variables>
                    </model>
                </xmile>
            |]

parseXMILE src =
    parse $ encodeUtf8 src