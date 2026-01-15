using System;
using System.Text;

namespace Occult
{
	internal class KObjects : IComparable
	{
		private long ePIC_ID;

		private double rA;

		private double dec;

		private double mag;

		private int cadence;

		private int xz;

		private string field;

		public static bool SortByXZ;

		public static bool SortByMag;

		public static bool SortByCadence;

		public long EPIC_ID
		{
			get
			{
				return ePIC_ID;
			}
			set
			{
				ePIC_ID = value;
			}
		}

		public double RA
		{
			get
			{
				return rA;
			}
			set
			{
				rA = value;
			}
		}

		public double Dec
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

		public double Mag
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

		public int Cadence
		{
			get
			{
				return cadence;
			}
			set
			{
				cadence = value;
			}
		}

		public string Field
		{
			get
			{
				return field;
			}
			set
			{
				field = value;
			}
		}

		public int XZ
		{
			get
			{
				return xz;
			}
			set
			{
				xz = value;
			}
		}

		public string K2Line
		{
			get
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append(EPIC_ID.ToString().PadLeft(9));
				stringBuilder.AppendFormat("{0,2:f0}", Field);
				stringBuilder.AppendFormat(" {0,10:f6}", RA);
				stringBuilder.AppendFormat(" {0,10:f6}", Dec);
				stringBuilder.AppendFormat(" {0,4:f1}", Mag);
				stringBuilder.AppendFormat(" {0,1:f0}", Cadence);
				return stringBuilder.ToString();
			}
		}

		public string K2LineNoFieldNum
		{
			get
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append(EPIC_ID.ToString().PadLeft(9));
				stringBuilder.AppendFormat(" {0,10:f6}", RA);
				stringBuilder.AppendFormat(" {0,10:f6}", Dec);
				stringBuilder.AppendFormat(" {0,4:f1}", Mag);
				stringBuilder.AppendFormat(" {0,1:f0}", Cadence);
				return stringBuilder.ToString();
			}
		}

		public int CompareTo(object other)
		{
			if (SortByXZ)
			{
				if (XZ == ((KObjects)other).XZ)
				{
					return RA.CompareTo(((KObjects)other).RA);
				}
				return XZ.CompareTo(((KObjects)other).XZ);
			}
			if (SortByMag)
			{
				if (Mag == ((KObjects)other).Mag)
				{
					return RA.CompareTo(((KObjects)other).RA);
				}
				return Mag.CompareTo(((KObjects)other).Mag);
			}
			if (SortByCadence)
			{
				if (Cadence == ((KObjects)other).Cadence)
				{
					return RA.CompareTo(((KObjects)other).RA);
				}
				return Cadence.CompareTo(((KObjects)other).Cadence);
			}
			if (RA == ((KObjects)other).RA)
			{
				return Dec.CompareTo(((KObjects)other).Dec);
			}
			return RA.CompareTo(((KObjects)other).RA);
		}
	}
}
