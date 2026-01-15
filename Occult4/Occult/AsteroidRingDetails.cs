using System;
using System.Text;

namespace Occult
{
	internal class AsteroidRingDetails : IComparable
	{
		private int iDAsteroidNumber;

		private int numberOfRings;

		private double rA_Pole;

		private double dec_Pole;

		private double radius1;

		private double radius2;

		private double radius3;

		private double radius4;

		private double radius5;

		public int IDAsteroidNumber
		{
			get
			{
				return iDAsteroidNumber;
			}
			set
			{
				iDAsteroidNumber = value;
			}
		}

		public double RA_Pole
		{
			get
			{
				return rA_Pole;
			}
			set
			{
				rA_Pole = value;
			}
		}

		public double Dec_Pole
		{
			get
			{
				return dec_Pole;
			}
			set
			{
				dec_Pole = value;
			}
		}

		public int NumberOfRings
		{
			get
			{
				return numberOfRings;
			}
			set
			{
				numberOfRings = value;
			}
		}

		public double Radius1
		{
			get
			{
				return radius1;
			}
			set
			{
				radius1 = value;
			}
		}

		public double Radius2
		{
			get
			{
				return radius2;
			}
			set
			{
				radius2 = value;
			}
		}

		public double Radius3
		{
			get
			{
				return radius3;
			}
			set
			{
				radius3 = value;
			}
		}

		public double Radius4
		{
			get
			{
				return radius4;
			}
			set
			{
				radius4 = value;
			}
		}

		public double Radius5
		{
			get
			{
				return radius5;
			}
			set
			{
				radius5 = value;
			}
		}

		internal void ReadElementLine(string LineIn)
		{
			string[] array = LineIn.Split(new char[1] { ',' });
			if (!int.TryParse(array[0], out iDAsteroidNumber))
			{
				iDAsteroidNumber = 0;
			}
			if (!double.TryParse(array[1], out rA_Pole))
			{
				rA_Pole = 0.0;
			}
			if (!double.TryParse(array[2], out dec_Pole))
			{
				dec_Pole = 0.0;
			}
			if (!int.TryParse(array[3], out numberOfRings))
			{
				numberOfRings = 0;
			}
			if (!double.TryParse(array[4], out radius1))
			{
				radius1 = 0.0;
			}
			if (!double.TryParse(array[5], out radius2))
			{
				radius2 = 0.0;
			}
			if (!double.TryParse(array[6], out radius3))
			{
				radius3 = 0.0;
			}
			if (!double.TryParse(array[7], out radius4))
			{
				radius4 = 0.0;
			}
			if (!double.TryParse(array[8], out radius5))
			{
				radius5 = 0.0;
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (iDAsteroidNumber > 0)
			{
				stringBuilder.AppendFormat("{0:F0},", iDAsteroidNumber);
			}
			else
			{
				stringBuilder.AppendFormat("".PadRight(7));
			}
			stringBuilder.AppendFormat("{0:F2},", rA_Pole);
			stringBuilder.AppendFormat("{0:F2},", dec_Pole);
			stringBuilder.AppendFormat("{0:F0},", numberOfRings);
			if (radius1 > 0.0)
			{
				stringBuilder.AppendFormat("{0:F1},", radius1);
			}
			else
			{
				stringBuilder.Append(",");
			}
			if (radius2 > 0.0)
			{
				stringBuilder.AppendFormat("{0:F1},", radius2);
			}
			else
			{
				stringBuilder.Append(",");
			}
			if (radius3 > 0.0)
			{
				stringBuilder.AppendFormat("{0:F1},", radius3);
			}
			else
			{
				stringBuilder.Append(",");
			}
			if (radius4 > 0.0)
			{
				stringBuilder.AppendFormat("{0:F1},", radius4);
			}
			else
			{
				stringBuilder.Append(",");
			}
			if (radius5 > 0.0)
			{
				stringBuilder.AppendFormat("{0:F1}", radius5);
			}
			return stringBuilder.ToString();
		}

		internal string RingLineForDisplay()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(IDAsteroidNumber.ToString().PadLeft(7));
			stringBuilder.AppendFormat(" {0,7:f2}{1,7:f2}{2,3:f0}", rA_Pole, dec_Pole, numberOfRings);
			stringBuilder.AppendFormat("{0,8:f1}", Radius1);
			stringBuilder.AppendFormat("{0,8:f1}", Radius2);
			stringBuilder.AppendFormat("{0,8:f1}", Radius3);
			stringBuilder.AppendFormat("{0,8:f1}", Radius4);
			stringBuilder.AppendFormat("{0,8:f1}", Radius5);
			return stringBuilder.ToString();
		}

		public int CompareTo(object other)
		{
			return iDAsteroidNumber.CompareTo(((AsteroidRingDetails)other).iDAsteroidNumber);
		}
	}
}
