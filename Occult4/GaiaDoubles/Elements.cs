using System;
using System.Text;
using System.Windows.Forms;
using Occult;
using Occult.Star_Catalogues;

namespace GaiaDoubles
{
	public class Elements : IComparable
	{
		public static GaiaDoubles_Display GaiaDoubles;

		private static string[] Catalog = new string[14]
		{
			"asb1c", "eb", "es", "oc", "oac", "oavc", "otsc", "otsvc", "sb1", "sb2",
			"sb1c", "sb2c", "acc7", "acc9"
		};

		private double ra;

		private double dec;

		private double mag;

		private double period;

		private double epoch;

		private double semimajor;

		private double ecc;

		private double inc;

		private double periastron;

		private double node;

		private double a;

		private double b;

		private double f;

		private double g;

		private double pmRA;

		private double pmDec;

		private double dpmRA;

		private double dpmDec;

		private double ddpmRA;

		private double ddpmDec;

		private double tprimary;

		private double tsecondary;

		private double durnprimary;

		private double durnsecondary;

		private int model;

		private string id;

		internal double RA
		{
			get
			{
				return ra;
			}
			set
			{
				ra = value;
			}
		}

		internal double Dec
		{
			get
			{
				return dec;
			}
			set
			{
				dec = value;
			}
		}

		internal double Mag
		{
			get
			{
				return mag;
			}
			set
			{
				mag = value;
			}
		}

		internal double Period
		{
			get
			{
				return period;
			}
			set
			{
				period = value;
			}
		}

		internal double Epoch
		{
			get
			{
				return epoch;
			}
			set
			{
				epoch = value;
			}
		}

		internal double Semimajor
		{
			get
			{
				return semimajor;
			}
			set
			{
				semimajor = value;
			}
		}

		internal double Ecc
		{
			get
			{
				return ecc;
			}
			set
			{
				ecc = value;
			}
		}

		internal double Inc
		{
			get
			{
				return inc;
			}
			set
			{
				inc = value;
			}
		}

		internal double Periastron
		{
			get
			{
				return periastron;
			}
			set
			{
				periastron = value;
			}
		}

		internal double Node
		{
			get
			{
				return node;
			}
			set
			{
				node = value;
			}
		}

		internal double A
		{
			get
			{
				return a;
			}
			set
			{
				a = value;
			}
		}

		internal double B
		{
			get
			{
				return b;
			}
			set
			{
				b = value;
			}
		}

		internal double F
		{
			get
			{
				return f;
			}
			set
			{
				f = value;
			}
		}

		internal double G
		{
			get
			{
				return g;
			}
			set
			{
				g = value;
			}
		}

		internal double Tprimary
		{
			get
			{
				return tprimary;
			}
			set
			{
				tprimary = value;
			}
		}

		internal double DurnPrimary
		{
			get
			{
				return durnprimary;
			}
			set
			{
				durnprimary = value;
			}
		}

		internal double Tsecondary
		{
			get
			{
				return tsecondary;
			}
			set
			{
				tsecondary = value;
			}
		}

		internal double DurnSecondary
		{
			get
			{
				return durnsecondary;
			}
			set
			{
				durnsecondary = value;
			}
		}

		internal double pm_RA
		{
			get
			{
				return pmRA;
			}
			set
			{
				pmRA = value;
			}
		}

		internal double pm_Dec
		{
			get
			{
				return pmDec;
			}
			set
			{
				pmDec = value;
			}
		}

		internal double dpm_RA
		{
			get
			{
				return dpmRA;
			}
			set
			{
				dpmRA = value;
			}
		}

		internal double dpm_Dec
		{
			get
			{
				return dpmDec;
			}
			set
			{
				dpmDec = value;
			}
		}

		internal double ddpm_RA
		{
			get
			{
				return ddpmRA;
			}
			set
			{
				ddpmRA = value;
			}
		}

		internal double ddpm_Dec
		{
			get
			{
				return ddpmDec;
			}
			set
			{
				ddpmDec = value;
			}
		}

		internal string ID
		{
			get
			{
				return id;
			}
			set
			{
				id = value;
			}
		}

		internal int Model
		{
			get
			{
				return model;
			}
			set
			{
				model = value;
			}
		}

		public static void Show_GaiaDoubles()
		{
			try
			{
				((Control)GaiaDoubles).Show();
			}
			catch
			{
				GaiaDoubles = new GaiaDoubles_Display();
				((Control)GaiaDoubles).Show();
			}
		}

		internal void TheileInnesToCampbell()
		{
			double num = A * A + B * B + F * F + G * G;
			double num2 = A * G - B * F;
			double num3 = Math.Sqrt(num * num - num2 * num2);
			double num4 = Math.Atan2(B + F, G - A);
			double num5 = Math.Atan2(G - A, B + F);
			Semimajor = Math.Sqrt(num3 + num);
			Periastron = (num4 + num5) / 2.0;
			Node = (num4 - num5) / 2.0;
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0:#.######},{1:#.######;-#.######},{2:#.##},", RA, Dec, Mag);
			stringBuilder.AppendFormat("{0:#.##;;},{1:#.#######;},{2:#.###},", Semimajor, Period, Epoch);
			stringBuilder.AppendFormat("{0:#.##;;},{1:#},{2:#;;},{3:#;;},", Ecc, Inc, Periastron, Node);
			stringBuilder.AppendFormat("{0:#.##;-#.##;},{1:#.##;-#.##;},{2:#.##;-#.##;},{3:#.##;-#.##;},", A, B, F, G);
			stringBuilder.AppendFormat("{0:#.###;-#.###;;},{1:#.###;-#.###;},{2:#.###;-#.###;},{3:#.###;-#.###;},", tprimary, durnprimary, tsecondary, durnsecondary);
			stringBuilder.AppendFormat("{0:#.###;-#.###;},{1:#.###;-#.###;},{2:#.###;-#.###;},{3:#.###;-#.###;},{4:#.###;-#.###;},{5:#.###;-#.###;},", pmRA, dpmRA, ddpmRA, pmDec, dpmDec, ddpmDec);
			stringBuilder.Append(ID.ToString() + "," + Catalog[Model]);
			return stringBuilder.ToString();
		}

		public static void SelectiveOut(string Line, bool DetailsOnly, out string Line1, out string Line2)
		{
			string[] array = new string[26]
			{
				"Right Asc.", "   Declination", "   Gmag   ", " Semimajor", " Period(d)", " Epoch(d)", " Ecc", " Inc", "Periastron", " Node",
				"A", "B", "F", "G", "EclP(d)", "DurnP(d)", "EclS(d)", "DurnS(d)", " pmRA", " dpmRA",
				" ddpmRA", " pmDec", " dpmDec", " ddpmDec", " Gaia Source number", " Model"
			};
			int[] array2 = new int[26]
			{
				13, 13, 8, 10, 13, 10, 6, 5, 11, 8,
				7, 7, 7, 7, 8, 9, 8, 9, 8, 8,
				8, 8, 8, 8, 21, 6
			};
			double result = 0.0;
			double result2 = 0.0;
			Line1 = (Line2 = "");
			string[] array3 = Line.Replace("\r", "").Split(new char[1] { ',' });
			double.TryParse(array3[0], out result);
			double.TryParse(array3[1], out result2);
			Line1 = array[0] + array[1] + array[2];
			Line2 = Utilities.DEGtoDMS(result / 15.0, 2, 2, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: false, IncludeHourDegLeadingZero: false).PadRight(13) + Utilities.DEGtoDMS(result2, 3, 1, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true, IncludeDMS: false, IncludeHMS: false, IncludeHourDegLeadingZero: false).PadRight(13) + array3[2].PadLeft(5).PadRight(8);
			for (int i = 3; i < array3.Length; i++)
			{
				if (!(DetailsOnly & ((i < 3) | (i == array3.Length - 2))) && array3[i] != "")
				{
					Line1 += array[i].PadLeft(array2[i]);
					Line2 += array3[i].PadLeft(array2[i]);
				}
			}
		}

		public int CompareTo(object other)
		{
			return RA.CompareTo(((Elements)other).RA);
		}
	}
}
