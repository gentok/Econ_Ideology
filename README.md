# 金融緩和実験レプリケーションデータ

## 日本における「ねじれ」た金融緩和選好を説明する：イデオロギーと政策選好の関係に情報環境が与える影響の実験的検証

Explaining "Twisted" Monetary Easing Preferences in Japan: Experimental Evidence on the Effect of Information Environment on the Relationship Between Ideology and Policy Preferences

* #### [ワーキングペーパー (4/10/2020)](paper/Econ_Ideology_Paper_v5_4_all.pdf)
* #### [オンライン付録 (4/5/2020)](paper/Econ_Ideology_Paper_v5_appendix.pdf)
<!-- * [ワーキングペーパー (9/6/2019)](paper/Econ_Ideology_Paper.pdf) -->

## 著者
KATO, Gento 加藤言人 (gento.badgerATgmail.com)<br>
ANNAKA, Susumu 安中進 (profoundATmoegi.waseda.jp)

## 概要
特定の政策において、日本で「左派」や「右派」と呼ばれる政党やその支持者は、欧米における左派や右派とは逆の「ねじれ」た選好を持つことが指摘されてきた。特に金融緩和政策では、緩和拡大に対し、欧米では左派が右派に比べて積極的な傾向がある一方で、日本では左派が反対する動きが根強い。この要因に関しては様々な議論があるが、経験的な検証は行われていない。本稿では日本の有権者を対象にサーベイ実験を行い、情報環境の側面からイデオロギーと金融緩和選好の関係を規定する要因を探る。実験では、特に貧困削減フレームと経済学者の賛成意見が同時に提示された条件下で、左派が右派と同程度かそれ以上に金融緩和を支持する傾向が見られた。結果は、日本におけるイデオロギーと政策選好の関係が欧米とは異なる背景について、情報環境が重要な役割を果たしていることを示唆している。

On specific policies, voters and political parties representing "left" and "right" in Japan are known to hold "twisted" policy preferences that conflict with left and right in western countries. Especially on monetary easing, in west, left tends to be more supportive of its expansion than right; In Japan, left is consistently opposing to this policy. Scholars provide various explanations for this pattern, but none has been assessed empirically. In the current study, we conduct a survey experiment of Japanese voters to explore the effect of information environment on the relationship between ideology and monetary easing preferences. The result shows that "left" voters support monetary easing the same or more than "right" voters if and only if both the "poverty reduction" framing and the endorsement from western economic experts are provided. The finding implies that information environment plays a critical role in explaining “twisted” Japanese policy preferences.

## 関連ファイル

分析を再現するには、レポジトリごとCloneもしくはダウンロードして、分析コードファイルを実行してください。分析コードはR言語で記述されています。相対パスでデータを読み込むためには、RStudioを使用してコードを実行してください。作成された図表は、<code>out</code>ディレクトリに出力されます。

* 実験データ（RDS形式） [data_public/main_data_v5.rds](data_public/main_data_v1.rds)
* 実験データ（STATA13形式） [data_public/main_data_v5.dta](data_public/main_data_v1.dta)

* 実験分析コードファイル [src/main_analysis_v5.R](src/main_analysis_v5.R)
* 実験分析コードファイルのアウトプット（オンライン） [src/main_analysis_v5.md](src/main_analysis_v5.md)

* 東大朝日調査分析コードファイル [src/utas2016_evidence_v5.R](src/utas2016_evidence_v5.R)
* 東大朝日調査コードファイルのアウトプット（オンライン） [src/utas2016_evidence_v5.md](src/utas2016_evidence_v5.md)

## ライセンス

The analytical results in this project are licensed under the [Creative Commons Attribution 4.0 license](https://choosealicense.com/licenses/cc-by-4.0/), and the programming code used to generate the result is licensed under the [MIT license](https://choosealicense.com/licenses/mit/).